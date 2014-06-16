--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Geometry
--
--------------------------------------------------------------------------------
module Dhek.Geometry where

--------------------------------------------------------------------------------
import Data.Foldable (find, foldMap)
import Data.Monoid (First(..))

--------------------------------------------------------------------------------
import Control.Lens
import Control.Monad.State.Strict
import Graphics.UI.Gtk (CursorType(..))

--------------------------------------------------------------------------------
import Dhek.Engine.Type
import Dhek.Types

--------------------------------------------------------------------------------
getOverRect :: DrawEnv -> (Maybe Rect)
getOverRect o =
    let (x,y) = drawPointer o
        rs    = drawRects o in
    find (isOver 1.0 x y) rs

--------------------------------------------------------------------------------
getOverArea :: DrawEnv -> (Maybe Area)
getOverArea o =
    let ratio = drawRatio o
        (x,y) = drawPointer o
        rOpt  = getOverRect o in
    rOpt >>= \r ->
        find (isOver 1.0 x y . rectArea (5/ratio) r) $ enumFrom TOP_LEFT

--------------------------------------------------------------------------------
isOver :: Double -> Double -> Double -> Rect -> Bool
isOver thick x y r = go
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

--------------------------------------------------------------------------------
adaptPos :: Direction
         -> Double
         -> Double
         -> Double
         -> (Double, Double)
adaptPos NORTH delta x y = (x, y-delta)
adaptPos SOUTH delta x y = (x, y-delta)
adaptPos WEST  delta x y = (x-delta, y)
adaptPos EAST  delta x y = (x-delta, y)

--------------------------------------------------------------------------------
adaptPosDefault :: Direction
                -> Double
                -> (Double, Double)
                -> (Double, Double)
                -> (Double, Double)
adaptPosDefault NORTH delta (dx,_) (_,y) = (dx, y-delta)
adaptPosDefault SOUTH delta (dx,_) (_,y) = (dx, y-delta)
adaptPosDefault WEST  delta (_,dy) (x,_) = (x-delta, dy)
adaptPosDefault EAST  delta (_,dy) (x,_) = (x-delta, dy)

--------------------------------------------------------------------------------
adaptRect :: Direction -> Double -> Rect -> Rect
adaptRect NORTH delta r = r & rectY -~ delta
adaptRect SOUTH delta r = r & rectY -~ delta
adaptRect WEST  delta r = r & rectX -~ delta
adaptRect EAST  delta r = r & rectX -~ delta

--------------------------------------------------------------------------------
rangeCollides :: Direction -> Double -> Double -> Rect -> Bool
rangeCollides d tmin tmax r = collides
  where
    rx = r ^. rectX
    ry = r ^. rectY
    rw = r ^. rectWidth
    rh = r ^. rectHeight

    collides =
        case d of
            NORTH -> tmin <= (rx+rw) && rx <= tmax
            SOUTH -> tmin <= (rx+rw) && rx <= tmax
            EAST  -> tmin <= (ry+rh) && ry <= tmax
            WEST  -> tmin <= (ry+rh) && ry <= tmax

--------------------------------------------------------------------------------
collisionDelta :: Direction -> Rect -> Rect -> Double
collisionDelta d l r = delta
  where
    lx = l ^. rectX
    ly = l ^. rectY
    lw = l ^. rectWidth
    lh = l ^. rectHeight

    rx = r ^. rectX
    ry = r ^. rectY
    rw = r ^. rectWidth
    rh = r ^. rectHeight

    delta =
        case d of
            NORTH -> ly + lh - ry
            WEST  -> lx + lw - rx
            SOUTH -> negate (ry + rh - ly)
            EAST  -> negate (rx + rw - lx)

--------------------------------------------------------------------------------
diffPos :: Direction -> Double -> (Double, Double) -> (Double, Double) -> Double
diffPos NORTH delta (_,y1) (_,y0) = y1 - y0 + delta
diffPos SOUTH delta (_,y1) (_,y0) = negate (y1 - y0 + delta)
diffPos WEST  delta (x1,_) (x0,_) = x1 - x0 + delta
diffPos EAST  delta (x1,_) (x0,_) = negate (x1 - x0 + delta)

--------------------------------------------------------------------------------
oppositeTranslate :: Direction
                  -> (Double, Double)
                  -> (Double, Double)
                  -> Rect
                  -> Rect
oppositeTranslate NORTH (x1,_) (x0,_) = translateRectX (x1-x0)
oppositeTranslate SOUTH (x1,_) (x0,_) = translateRectX (x1-x0)
oppositeTranslate WEST  (_,y1) (_,y0) = translateRectY (y1-y0)
oppositeTranslate EAST  (_,y1) (_,y0) = translateRectY (y1-y0)

--------------------------------------------------------------------------------
updateHoldPos :: Direction
              -> (Double, Double)
              -> (Double, Double)
              -> (Double, Double)
updateHoldPos NORTH (x1,_) (_,y0) = (x1,y0)
updateHoldPos SOUTH (x1,_) (_,y0) = (x1,y0)
updateHoldPos WEST (_,y1) (x0,_)  = (x0,y1)
updateHoldPos EAST (_,y1) (x0,_)  = (x0,y1)

--------------------------------------------------------------------------------
correctRect :: Direction -> (Double, Double) -> Rect -> (Rect, (Double, Double))
correctRect NORTH (x,y) r = (translateRectY (negate 1) r, (x, y-1))
correctRect SOUTH (x,y) r = (translateRectY 1 r, (x, y+1))
correctRect WEST  (x,y) r = (translateRectX (negate 1) r, (x-1, y))
correctRect EAST  (x,y) r = (translateRectX 1 r, (x+1, y))

--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
updateDrawSelection :: Pos -> Rect -> Rect
updateDrawSelection (x,y) = execState go where
  go = do
      sx <- use rectX
      sy <- use rectY
      rectWidth  .= x-sx
      rectHeight .= y-sy

--------------------------------------------------------------------------------
updateHoldRect :: Pos -> Pos -> Rect -> Rect
updateHoldRect (x0,y0) (x,y) = execState go where
  go = do
      rectX += x-x0
      rectY += y-y0

--------------------------------------------------------------------------------
replaceRect :: Direction -> Rect -> Rect -> (Double, Rect)
replaceRect d l r  = r1
  where
    lx = l ^. rectX
    ly = l ^. rectY
    lw = l ^. rectWidth
    lh = l ^. rectHeight

    rx = r ^. rectX
    ry = r ^. rectY
    rw = r ^. rectWidth
    rh = r ^. rectHeight

    r1 =
        case d of
            NORTH -> let e = ly + lh - ry in (e, l & rectY -~ e)
            WEST  -> let e = lx + lw - rx in (e, l & rectX -~ e)
            SOUTH -> let e = ry + rh - ly in (e, l & rectY +~ e)
            EAST  -> let e = rx + rw - lx in (e, l & rectX +~ e)

--------------------------------------------------------------------------------
intersection :: [Rect] -> Rect -> Maybe (Rect, Direction)
intersection rs l = getFirst $ foldMap (First . go) rs
  where
    go r = do
        dir <- rectIntersect l r
        return (r, dir)

--------------------------------------------------------------------------------
rectRange :: Direction -> Rect -> (Double, Double)
rectRange d r =
    case d of
        NORTH -> (x, x+w)
        EAST  -> (y, y+h)
        SOUTH -> (x, x+w)
        WEST  -> (y, y+h)
  where
    x = r ^. rectX
    y = r ^. rectY
    w = r ^. rectWidth
    h = r ^. rectHeight

--------------------------------------------------------------------------------
fromEdge :: Direction -> (Double, Double) -> Rect -> Double
fromEdge d (x,y) r =
    case d of
        NORTH -> y - ry
        SOUTH -> y - (ry+rh)
        WEST  -> x - rx
        EAST  -> x - (rx+rw)
  where
    rx = r ^. rectX
    ry = r ^. rectY
    rw = r ^. rectWidth
    rh = r ^. rectHeight

--------------------------------------------------------------------------------
eventCursor :: BoardEvent -> CursorType
eventCursor (Hold _ _)     = Hand1
eventCursor (Resize _ _ a) = areaCursor a

--------------------------------------------------------------------------------
areaCursor :: Area -> CursorType
areaCursor TOP_LEFT     = TopLeftCorner
areaCursor TOP          = TopSide
areaCursor TOP_RIGHT    = TopRightCorner
areaCursor RIGHT        = RightSide
areaCursor BOTTOM_RIGHT = BottomRightCorner
areaCursor BOTTOM       = BottomSide
areaCursor BOTTOM_LEFT  = BottomLeftCorner
areaCursor LEFT         = LeftSide

--------------------------------------------------------------------------------
isOverGuide :: Double -> DrawEnv -> Guide -> Bool
isOverGuide gRange opts (Guide pos typ)
    = case typ of
    GuideVertical   -> pos-gRange <= x && x <= pos+gRange
    GuideHorizontal -> pos-gRange <= y && y <= pos+gRange
  where
    (x,y) = drawPointer opts
