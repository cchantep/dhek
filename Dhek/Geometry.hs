--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Geometry
--
--------------------------------------------------------------------------------
module Dhek.Geometry where

--------------------------------------------------------------------------------
import Data.Foldable (find)

--------------------------------------------------------------------------------
import Control.Lens
import Control.Monad.State.Strict
import Graphics.UI.Gtk (CursorType(..))

--------------------------------------------------------------------------------
import Dhek.Cartesian
import Dhek.Engine.Type
import Dhek.Types

--------------------------------------------------------------------------------
getOverRect :: DrawEnv -> (Maybe Rect)
getOverRect o =
    let pt = drawPointer o
        rs = drawRects o in
    find (isOver 1.0 pt) rs

--------------------------------------------------------------------------------
getOverArea :: DrawEnv -> (Maybe Area)
getOverArea o =
    let ratio = drawRatio o
        pt    = drawPointer o
        rOpt  = getOverRect o in
    rOpt >>= \r ->
        find (isOver 1.0 pt . rectArea (5/ratio) r) $ enumFrom TOP_LEFT

--------------------------------------------------------------------------------
isOver :: Double -> Point2D -> Rect -> Bool
isOver thick pt r = go
  where
    x0 = r ^. rectX
    y0 = r ^. rectY
    h  = r ^. rectHeight
    w  = r ^. rectWidth
    x1 = (x0 + w + thick)
    y1 = (y0 + h + thick)
    x  = pt ^. pointX
    y  = pt ^. pointY
    x' = (x0 - thick)
    y' = (y0 - thick)

    go = x >= x' && x <= x1 && y >= y' && y <= y1

--------------------------------------------------------------------------------
resizeRect :: Area -> Vector2D -> Rect -> Rect
resizeRect a v r = execState (go a) r
  where
    go TOP_LEFT
        = do rectX      += dx
             rectY      += dy
             rectWidth  -= dx
             rectHeight -= dy
    go TOP
        = do rectY      += dy
             rectHeight -= dy
    go TOP_RIGHT
        = do rectY      += dy
             rectWidth  += dx
             rectHeight -= dy
    go RIGHT
        = rectWidth += dx
    go BOTTOM_RIGHT
        = do rectWidth  += dx
             rectHeight += dy
    go BOTTOM
        = rectHeight += dy
    go BOTTOM_LEFT
        = do rectX      += dx
             rectWidth  -= dx
             rectHeight += dy
    go LEFT
        = do rectX     += dx
             rectWidth -= dx

    dx = vector2DDeltaX v
    dy = vector2DDeltaY v

--------------------------------------------------------------------------------
vector2DRect :: Vector2D -> Rect
vector2DRect v = rectNew fpt height width
  where
    fpt    = v ^. vectorFrom
    width  = vector2DWidth v
    height = vector2DHeight v

--------------------------------------------------------------------------------
updateDrawSelection :: Pos -> Rect -> Rect
updateDrawSelection (x,y) = execState go where
  go = do
      sx <- use rectX
      sy <- use rectY
      rectWidth  .= x-sx
      rectHeight .= y-sy

--------------------------------------------------------------------------------
updateDrawSelectionX :: Double -> Rect -> Rect
updateDrawSelectionX x = execState go where
  go = do sx <- use rectX
          rectWidth .= x-sx

--------------------------------------------------------------------------------
updateDrawSelectionY :: Double -> Rect -> Rect
updateDrawSelectionY y = execState go where
  go = do sy <- use rectY
          rectHeight .= y-sy

--------------------------------------------------------------------------------
moveRect :: Vector2D -> Rect -> Rect
moveRect vect = execState go where
  go = do
      rectX += x-x0
      rectY += y-y0

  pt0 = vect ^. vectorFrom
  pt  = vect ^. vectorTo
  x0  = pt0  ^. pointX
  y0  = pt0  ^. pointY
  x   = pt   ^. pointX
  y   = pt   ^. pointY

--------------------------------------------------------------------------------
makeDrawSelectionRect :: Vector2D -> Rect
makeDrawSelectionRect v = rect
  where
    v1 | vector2DBackward v
         = if vector2DUpward v then swapVector2D v else swapXVector2D v
       | otherwise
         = if vector2DUpward v then swapYVector2D v else v

    rect = vector2DRect v1

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
    pt = drawPointer opts
    x  = pt ^. pointX
    y  = pt ^. pointY
