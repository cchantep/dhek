{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Dhek.Types where

import Control.Applicative ((<*>), (<$>))
import Control.Lens hiding ((.=), get)
import Control.Monad (mzero)
import Control.Monad.State (execState, evalStateT, modify, get)
import Control.Monad.Trans (lift)
import Data.Array (Array)
import Data.Aeson hiding (Array)
import Data.Aeson.Types (Parser)
import Data.Foldable (foldMap)
import Data.IntMap (IntMap, alter, empty, fromList)
import Data.Monoid (Monoid (..))
import qualified Data.Vector as V
import Dhek.Version
import Graphics.UI.Gtk hiding (get)
import Graphics.UI.Gtk.Poppler.Document (Document, Page)

data Viewer = Viewer { _viewerDocument  :: Document
                     , _viewerPages     :: Array Int PageItem
                     , _viewerPageCount :: Int
                     }

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

data Direction = NORTH
               | EAST
               | SOUTH
               | WEST deriving (Eq, Show)

data Boards = Boards { _boardsState     :: {-# UNPACK #-} !Int
                     , _boardsEvent     :: !BoardEvent
                     , _boardsSelection :: !(Maybe Rect)
                     , _boardsOvered    :: !(Maybe Rect)
                     , _boardsSelected  :: !(Maybe Rect)
                     , _boardsCurGuide  :: !(Maybe Guide)
                     , _boardsGuides    :: ![Guide]
                     , _boardsMap       :: !(IntMap Board) }

data Rect = Rect { _rectId     :: {-# UNPACK #-} !Int
                 , _rectX      :: {-# UNPACK #-} !Double
                 , _rectY      :: {-# UNPACK #-} !Double
                 , _rectHeight :: {-# UNPACK #-} !Double
                 , _rectWidth  :: {-# UNPACK #-} !Double
                 , _rectName   :: !String
                 , _rectType   :: !String
                 , _rectValue  :: !(Maybe String)
                 } deriving (Eq, Show)

data GuideType = GuideVertical | GuideHorizontal

data Guide = Guide { _guideValue :: {-# UNPACK #-} !Double
                   , _guideType  :: !GuideType }

data Save = Save { saveVersion :: !String
                 , saveAreas   :: ![(Int, Maybe [Rect])] }

data PageItem = PageItem
    { pagePtr    :: !Page
    , pageWidth  :: {-# UNPACK #-} !Double
    , pageHeight :: {-# UNPACK #-} !Double }

makeLenses ''Viewer
makeLenses ''Board
makeLenses ''Boards
makeLenses ''Rect
makeLenses ''Guide

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
        object $ maybe props (const $ vProp:props) (_rectValue r)
      where
        vProp = "value" .= _rectValue r

        props = [ "x"      .= _rectX r
                , "y"      .= _rectY r
                , "height" .= _rectHeight r
                , "width"  .= _rectWidth r
                , "name"   .= _rectName r
                , "type"   .= _rectType r
                ]

instance FromJSON Rect where
    parseJSON (Object v) =
        Rect 0         <$>
        v .:  "x"      <*>
        v .:  "y"      <*>
        v .:  "height" <*>
        v .:  "width"  <*>
        v .:  "name"   <*>
        v .:  "type"   <*>
        v .:? "value"
    parseJSON _ = mzero

saveNew :: [(Int, Maybe [Rect])] -> Save
saveNew = Save dhekFullVersion

boardsNew :: Int -> Boards
boardsNew n = Boards 0 None Nothing Nothing Nothing Nothing [] maps
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
rectNew x y h w = Rect 0 x y h w "field" "text" Nothing

translateRect :: Double -> Double -> Rect -> Rect
translateRect x y r = r & rectX +~ x & rectY +~ y

translateRectX :: Double -> Rect -> Rect
translateRectX x r = r & rectX +~ x

translateRectY :: Double -> Rect -> Rect
translateRectY y r = r & rectY +~ y

eventGetRect :: BoardEvent -> Maybe Rect
eventGetRect (Hold r _)     = Just r
eventGetRect (Resize r _ _) = Just r
eventGetRect _              = Nothing

eventSetRect :: Rect -> BoardEvent -> BoardEvent
eventSetRect r (Hold _ x)     = Hold r x
eventSetRect r (Resize _ x y) = Resize r x y
eventSetRect _ e              = e

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

-- | Using Minkowski Sum
rectIntersect :: Rect -> Rect -> Maybe Direction
rectIntersect a b
    | not collides = Nothing
    | otherwise =
        if wy > hx
        then if wy > negate hx
             then Just SOUTH
             else Just WEST
        else if wy > negate hx
             then Just EAST
             else Just NORTH
  where
    aw = a ^. rectWidth
    ah = a ^. rectHeight
    ax = a ^. rectX
    ay = a ^. rectY
    acenterx = ax + aw / 2
    acentery = ay + ah / 2

    bw = b ^. rectWidth
    bh = b ^. rectHeight
    bx = b ^. rectX
    by = b ^. rectY
    bcenterx = bx + bw / 2
    bcentery = by + bh / 2

    w  = 0.5 * (aw + bw)
    h  = 0.5 * (ah + bh)
    dx = acenterx - bcenterx
    dy = acentery - bcentery

    collides = abs dx <= w && abs dy <= h

    wy = w * dy
    hx = h * dx

modifyEventRect :: (Rect -> Rect) -> BoardEvent -> BoardEvent
modifyEventRect k (Hold r p)     = Hold (k r) p
modifyEventRect k (Resize r p a) = Resize (k r) p a

oppositeDirection :: Direction -> Direction
oppositeDirection NORTH = SOUTH
oppositeDirection SOUTH = NORTH
oppositeDirection EAST  = WEST
oppositeDirection WEST  = EAST

--------------------------------------------------------------------------------
sameRectId :: Rect -> Rect -> Bool
sameRectId r x = (x ^. rectId) == (r ^. rectId)
