--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Move
--
-- Drawing Area pointer interaction
--------------------------------------------------------------------------------
module Dhek.Move2 where

--------------------------------------------------------------------------------
import Control.Applicative ((<|>), (<$))
import Control.Monad ((<=<), when)
import Data.Foldable (foldMap, for_, traverse_)
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid (First(..))
import Data.Traversable (traverse)

--------------------------------------------------------------------------------
import Control.Lens (use, (.=), (%=), (?=), (+=), (-=), (<%=), (^.), (&), (.~), (%~), (-~), (+~))
import Control.Monad.Reader (ask)
import Control.Monad.State (execState)
import Graphics.UI.Gtk (CursorType(..))

--------------------------------------------------------------------------------
import Dhek.Drawing
import Dhek.Free
import Dhek.Instr
import Dhek.Types hiding (addRect)

--------------------------------------------------------------------------------
-- | Event handlers
--------------------------------------------------------------------------------
onMove :: Drawing ()
onMove = do
    oOpt <- getOverRect
    aOpt <- getOverArea
    eOpt <- use drawEvent
    sOpt <- use drawSelection

    -- When user draws a rectangle
    for_ sOpt $ \s -> do
        (x,y) <- getPointer
        drawSelection ?= flip execState s $ do
            sx <- use rectX
            sy <- use rectY
            rectWidth  .= x-sx
            rectHeight .= y-sy

    -- When user resizes or moves a rectangle
    for_ eOpt $ \e -> do
        (x,y)   <- getPointer
        overlap <- overlapEnabled
        if overlap
            then overlapMode e
            else collisionMode e

    let cursorOpt =  fmap eventCursor eOpt <|>
                     fmap areaCursor aOpt  <|>
                     Hand1 <$ oOpt

    drawCursor .= cursorOpt

  where
    overlapMode :: BoardEvent -> Drawing ()
    overlapMode e = do
        (x,y) <- getPointer
        case e of
            Hold r (x0,y0) ->
                drawEvent ?= flip Hold (x,y) $ flip execState r $ do
                    rectX += x-x0
                    rectY += y-y0

            Resize r (x0,y0) a ->
                let dx = x-x0
                    dy = y-y0 in
                drawEvent ?= Resize (resizeRect dx dy r) (x,y) a

    collisionMode :: BoardEvent -> Drawing ()
    collisionMode e = do
        (x,y) <- getPointer
        case e of
            Hold r (x0,y0) -> do
                cOpt <- use drawCollision
                case cOpt of
                    Nothing -> do -- No previous collision
                        ts <- getRects
                        case intersection ts r of
                            Nothing    -> overlapMode e
                            Just (t,d) -> do
                                let delta = collisionDelta d r t
                                    r1    = adapRect d delta r
                                    c     = Collision
                                            { colDelta     = delta
                                            , colRange     = rectRange d t
                                            , colPrevPos   = (x0,y0)
                                            , colDirection = d
                                            }
                                drawEvent     ?= Hold r1 (adaptPos d delta x y)
                                drawCollision ?= c

                    Just c -> do -- with previous collision
                        let (rmin,rmax) = colRange c
                            (px,py)     = colPrevPos c
                            d           = colDirection c
                            delta       = colDelta c
                            collides    = rangeCollides d rmin rmax r
                            diff        = diffPos d delta (x,y) (px,py)
                            (x0',y0')   = adaptPosDefault d delta (x,y) (x0,y0)
                            r1          = oppositeTranslate d (x,y) (x0,y0) r
                            (px',py')   = updateHoldPos d (x,y) (px,py)
                            catchUp     = diff <= 0

                        if not catchUp && collides
                            then drawEvent ?= Hold r1 (px',py')
                            else do
                                drawCollision .= Nothing
                                let (r2, newPos) = correctRect d (x0',y0') r
                                drawEvent ?= Hold r2 newPos

            _ -> overlapMode e -- Resize collision detection is
                               -- not supported yet.

--------------------------------------------------------------------------------
onPress :: Drawing ()
onPress = do
    (x,y) <- getPointer
    oOpt  <- getOverRect
    aOpt  <- getOverArea

    let newSelection = rectNew x y 0 0

        onEvent r = do
            let evt = maybe (Hold r (x,y)) (Resize r (x,y)) aOpt
            drawEvent    ?= evt
            drawDetached ?= r

    -- if user click on a blank area we're un drawing mode otherwise we enter
    -- event mode (Resize or Hold).
    maybe (drawSelection ?= newSelection) onEvent oOpt

--------------------------------------------------------------------------------
onRelease :: Drawing ()
onRelease = do
    sOpt <- use drawSelection
    eOpt <- use drawEvent

    -- on event mode, we re-insert targeted rectangle rectangle list
    for_ eOpt $ \e -> do
        let r = case e of
                Hold x _     -> x
                Resize x _ _ -> x

        drawSelected  ?= normalize r
        drawEvent     .= Nothing
        drawCollision .= Nothing

    -- on drawing mode, we add the new rectangle to rectangle list
    for_ sOpt $ \r -> do
        let r1 = normalize r
            w  = r1 ^. rectWidth
            h  = r1 ^. rectHeight

        when (w*h >= 30) $ do
            id <- drawFreshId <+= 1
            let r2 = r1 & rectId   .~ id
                        & rectName %~ (++ show id)

            drawNewRect  ?= r3
            drawSelected ?= r2

        drawSelection .= Nothing

--------------------------------------------------------------------------------
-- | Geometry utilities
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
