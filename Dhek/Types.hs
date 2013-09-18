{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Dhek.Types where

import Control.Arrow (first)
import Control.Lens hiding ((.=))
import Control.Monad.State (execState)
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.Foldable (foldMap)
import Data.IntMap (IntMap, alter, empty, fromList)
import Data.Monoid (Monoid (..))
import Data.String (fromString)
import Dhek.Version
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Poppler.Document (Document)

data Viewer = Viewer { _viewerDocument    :: Document
                     , _viewerCurrentPage :: Int
                     , _viewerPageCount   :: Int
                     , _viewerBoards      :: Boards }

data Board = Board { _boardRects :: IntMap Rect }

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

data Boards = Boards { _boardsState        :: Int
                     , _boardsEvent        :: BoardEvent
                     , _boardsSelection    :: Maybe Rect
                     , _boardsThick        :: Double
                     , _boardsArea         :: DrawingArea
                     , _boardsScrollWindow :: ScrolledWindow
                     , _boardsSelected     :: Maybe Int
                     , _boardsZoom         :: Int
                     , _boardsBaseWidth    :: Int
                     , _boardsMap          :: IntMap Board }

data Rect = Rect { _rectId     :: Int
                 , _rectX      :: Double
                 , _rectY      :: Double
                 , _rectHeight :: Double
                 , _rectWidth  :: Double
                 , _rectName   :: String
                 , _rectType   :: String } deriving (Eq, Show)

data Save = Save { saveAreas :: [(Int, Maybe [Rect])] }

data Field = Field { fieldRect  :: Rect
                   , fieldType  :: String
                   , fieldValue :: String }

instance Monoid Board where
  mempty = Board empty
  mappend (Board l) (Board r) = Board (mappend l r)

instance ToJSON Save where
  toJSON (Save areas) =
    let toPage (_, rects) = maybe Null (\t -> object ["areas" .= t]) rects
        pages = fmap toPage areas in
    object ["format" .= dhekFullVersion, "pages" .= pages]

$(deriveJSON (fmap toLower . drop 4) ''Rect)
makeLenses ''Viewer
makeLenses ''Board
makeLenses ''Boards
makeLenses ''Rect

boardsNew :: Int -- page count
          -> DrawingArea
          -> ScrolledWindow
          -> Int -- base width
          -> Int -- zoom
          -> Double -- thick
          -> Boards
boardsNew nb area win bw z th =
  Boards 0 None Nothing th area win Nothing z bw boards
  where
    boards = fromList $ fmap (\i -> (i, Board empty)) [1..nb]

fillUp :: Int -> [(Int, [Rect])] -> [(Int, Maybe [Rect])]
fillUp n xs = go xs [0..(n - 1)]
  where
    go [] is = fmap (\i -> (i, Nothing)) is
    go v@((k, rs):xs) (i:is)
       | k == i = (k, Just rs) : go xs is
       | k > i  = (i, Nothing) : go v is

rectNew :: Double -> Double -> Double -> Double -> Rect
rectNew x y h w = Rect 0 x y h w "field" "text/checkbox"

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
