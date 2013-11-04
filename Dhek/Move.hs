module Dhek.Move where

import Control.Lens (use, (.=), (%=), (^.), (+=), (-=), (<%=))
import Control.Monad ((<=<))
import Control.Monad.Reader (ask)
import Control.Monad.State (execState)

import Data.Foldable (traverse_)
import Data.Maybe (isJust)
import Data.Traversable (traverse)

import Dhek.Engine
import Dhek.Types

import Graphics.UI.Gtk.Gdk.Cursor (CursorType (..))

onMove :: EngineCallback Move
onMove (Move x y) = do
    env <- ask
    let oOpt = _engineOverRect env
        aOpt = _engineOverArea env
    sOpt <- engineSelection <%= fmap (execState selection)
    eOpt <- engineEvent     <%= fmap event
    cursor eOpt
    engineDraw .= (isJust sOpt || isJust eOpt)
  where
    selection = do
        x0 <- use rectX
        y0 <- use rectY
        rectWidth  .= x - x0
        rectHeight .= y - y0

    event (Hold r (x0,y0))     = Hold (translateRect (x-x0) (y-y0) r) (x,y)
    event (Resize r (x0,y0) a) = Resize (resizeRect (x-x0) (y-y0) a r) (x,y) a

    cursor eOpt =
        let cOpt = fmap eventCursor eOpt in
        engineCursor .= cOpt

resizeRect :: Double -> Double -> Area -> Rect -> Rect
resizeRect dx dy area r = execState (go area) r
  where
    go TOP_LEFT = do
        rectX += dx
        rectY += dy
        rectWidth  -= dx
        rectHeight -= dy
    go TOP = do
        rectY += dy
        rectHeight -= dy
    go TOP_RIGHT = do
        rectY += dy
        rectWidth  += dx
        rectHeight -= dy
    go RIGHT = do
        rectWidth += dx
    go BOTTOM_RIGHT = do
        rectWidth += dx
        rectHeight += dy
    go BOTTOM = do
        rectHeight += dy
    go BOTTOM_LEFT = do
        rectX += dx
        rectWidth -= dx
        rectHeight += dy
    go LEFT = do
        rectX += dx
        rectWidth -= dx

areaCursor :: Area -> CursorType
areaCursor TOP_LEFT     = TopLeftCorner
areaCursor TOP          = TopSide
areaCursor TOP_RIGHT    = TopRightCorner
areaCursor RIGHT        = RightSide
areaCursor BOTTOM_RIGHT = BottomRightCorner
areaCursor BOTTOM       = BottomSide
areaCursor BOTTOM_LEFT  = BottomLeftCorner
areaCursor LEFT         = LeftSide

eventCursor :: BoardEvent -> CursorType
eventCursor (Hold _ _)     = Hand1
eventCursor (Resize _ _ a) = areaCursor a
