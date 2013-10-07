{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Dhek.Types where

import Control.Applicative ((<*>), (<$>))
import Control.Arrow (first)
import Control.Lens hiding ((.=), get)
import Control.Monad (mzero)
import Control.Monad.State (execState, evalStateT, modify, get)
import Control.Monad.Trans (lift)
import Data.Array
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Aeson.TH
import Data.Char
import Data.Foldable (foldMap)
import Data.IntMap (IntMap, alter, empty, fromList)
import Data.Monoid (Monoid (..))
import Data.String (fromString)
import qualified Data.Vector as V
import Dhek.Version
import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Poppler.Document (Document, Page)

data Viewer = Viewer { _viewerDocument     :: Document
                     , _viewerCurrentPage  :: Int
                     , _viewerPageCount    :: Int
                     , _viewerBaseWidth    :: Int
                     , _viewerZoom         :: Int
                     , _viewerThick        :: Double
                     , _viewerBoards       :: Boards }

data Board = Board { _boardRects :: !(IntMap Rect) } deriving Show

data BoardEvent = None
                | Hold Rect (Double, Double)
                | Resize Rect (Double, Double) Area deriving (Show, Eq)

data Area = TOP_LEFT
          | TOP
          | TOP_RIGHT
          | RIGHT
          | BOTTOM_RIGHT
          | BOTTOM
          | BOTTOM_LEFT
          | LEFT deriving (Enum, Show, Eq)

data Boards = Boards { _boardsState     :: {-# UNPACK #-} !Int
                     , _boardsEvent     :: !BoardEvent
                     , _boardsSelection :: !(Maybe Rect)
                     , _boardsOvered    :: !(Maybe Int)
                     , _boardsSelected  :: !(Maybe Int)
                     , _boardsMap       :: !(IntMap Board) }

data Rect = Rect { _rectId     :: {-# UNPACK #-} !Int
                 , _rectX      :: {-# UNPACK #-} !Double
                 , _rectY      :: {-# UNPACK #-} !Double
                 , _rectHeight :: {-# UNPACK #-} !Double
                 , _rectWidth  :: {-# UNPACK #-} !Double
                 , _rectName   :: !String
                 , _rectType   :: !String } deriving (Eq, Show)

data Save = Save { saveVersion :: !String
                 , saveAreas   :: ![(Int, Maybe [Rect])] }

data PageItem = PageItem
    { _pagePtr    :: !Page
    , _pageWidth  :: {-# UNPACK #-} !Double
    , _pageHeight :: {-# UNPACK #-} !Double }

makeLenses ''Viewer
makeLenses ''Board
makeLenses ''Boards
makeLenses ''Rect

instance Monoid Board where
    mempty = Board empty
    mappend (Board l) (Board r) = Board (mappend l r)

instance ToJSON Save where
    toJSON (Save v areas) =
        let toPage (_, rects) = maybe Null (\t -> object ["areas" .= t]) rects
            pages             = fmap toPage areas in
        object ["format" .= v, "pages" .= pages]

instance FromJSON Save where
    parseJSON (Object v) = do
      ver   <- v .: "format"
      pages <- v .: "pages"
      areas <- withArray "list of pages" (go . V.toList) pages
      return (Save ver areas)
        where
          go xs = evalStateT (traverse pageToAreas xs) 0

          pageToAreas opt = do
            modify (+1)
            i <- get
            case opt of
              Null -> return (i, Nothing)
              _    -> do
                  xs <- lift $ withObject "page" extractAreas opt
                  return (i, Just xs)

          extractAreas obj = do
            areas <- obj .: "areas"
            withArray "areas" (toAreas . V.toList) areas

          toAreas = traverse parseJSON
    parseJSON _          = mzero

instance ToJSON Rect where
    toJSON r =
        object ["x"      .= _rectX r
               ,"y"      .= _rectY r
               ,"height" .= _rectHeight r
               ,"width"  .= _rectWidth r
               ,"name"   .= _rectName r
               ,"type"   .= _rectType r]

instance FromJSON Rect where
    parseJSON (Object v) =
        Rect 0        <$>
        v .: "x"      <*>
        v .: "y"      <*>
        v .: "height" <*>
        v .: "width"  <*>
        v .: "name"   <*>
        v .: "type"
    parseJSON _ = mzero

saveNew :: [(Int, Maybe [Rect])] -> Save
saveNew = Save dhekFullVersion

boardsNew :: Int -> Boards
boardsNew n = Boards 0 None Nothing Nothing Nothing maps
    where
      maps = fromList $ fmap (\i -> (i, Board empty)) [1..n]

fillUp :: Int -> [(Int, [Rect])] -> [(Int, Maybe [Rect])]
fillUp n xs = go xs [1..n]
    where
      go [] is = fmap (\i -> (i, Nothing)) is
      go _ []  = []
      go v@((k, rs):xs) (i:is)
          | k == i = (k, bool (null rs) Nothing (Just rs)) : go xs is
          | k > i  = (i, Nothing) : go v is

bool :: Bool -> a -> a -> a
bool True x _  = x
bool False _ y = y

rectNew :: Double -> Double -> Double -> Double -> Rect
rectNew x y h w = Rect 0 x y h w "field" "text"

translateRect :: Double -> Double -> Rect -> Rect
translateRect x y r = r & rectX +~ x & rectY +~ y

eventGetRect :: BoardEvent -> Maybe Rect
eventGetRect (Hold r _)     = Just r
eventGetRect (Resize r _ _) = Just r
eventGetRect _              = Nothing

normalize :: Rect -> Rect
normalize r = newRectY newRectX
    where
      x = _rectX r
      y = _rectY r
      w = _rectWidth r
      h = _rectHeight r

      newRectX
          | w < 0     = r { _rectX = x + w, _rectWidth = abs w }
          | otherwise = r

      newRectY r
          | h < 0     = r { _rectY = y + h, _rectHeight = abs h }
          | otherwise = r

addRect :: Int -> Rect -> Boards -> Boards
addRect page x = execState action
    where
      action = do
        i <- use boardsState
        boardsState += 1
        let x' = x & rectId .~ i & rectName %~ (++ show i)
        (boardsMap.at page.traverse.boardRects.at i) ?= x'

rectArea :: Double -> Rect -> Area -> Rect
rectArea eps r area = go area
    where
      x = r ^. rectX
      y = r ^. rectY
      h = r ^. rectHeight
      w = r ^. rectWidth

      go TOP_LEFT     = rectNew x y eps eps
      go TOP          = rectNew (x+eps) y eps (w-2*eps)
      go TOP_RIGHT    = rectNew (x+w-eps) y eps eps
      go RIGHT        = rectNew (x+w-eps) (y+eps) (h-2*eps) eps
      go BOTTOM_RIGHT = rectNew (x+w-eps) (y+h-eps) eps eps
      go BOTTOM       = rectNew (x+eps) (y+h-eps) eps (w-2*eps)
      go BOTTOM_LEFT  = rectNew x (y+h-eps) eps eps
      go LEFT         = rectNew x (y+eps) (h-2*eps) eps

isOver :: Double -> Double -> Double -> Double -> Rect -> Bool
isOver ratio thick x y r = go
    where
      x0 = r ^. rectX
      y0 = r ^. rectY
      h  = r ^. rectHeight
      w  = r ^. rectWidth
      x1 = (x0 + w + thick)
      y1 = (y0 + h + thick)
      x' = (x0 - thick)
      y' = (y0 - thick)

      go = x >= x' && x <= x1 && y >= y' && y <= y1
