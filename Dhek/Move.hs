module Dhek.Move where

import Control.Applicative ((<|>), (<$))
import Control.Lens (use, (.=), (%=), (?=), (+=), (-=), (<%=), (^.), (&), (.~), (%~), (-~), (+~))
import Control.Monad ((<=<), when)
import Control.Monad.Reader (ask)
import Control.Monad.State (execState)

import Data.Foldable (foldMap, for_, traverse_)
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid (First(..))
import Data.Traversable (traverse)

import Graphics.UI.Gtk (CursorType(..))

import Dhek.Instr
import Dhek.Types hiding (addRect)

import Debug.Trace

onMove :: DhekProgram ()
onMove = compile $ do
    oOpt <- getOverRect
    aOpt <- getOverArea
    eOpt <- getEvent
    sOpt <- getSelection
    traverse_ onSelection sOpt
    traverse_ onEvent eOpt
    let cOpt = fmap eventCursor eOpt <|>
               fmap areaCursor aOpt  <|>
               Hand1 <$ oOpt
    setCursor cOpt

  where
    onSelection r = do
        (x,y) <- getPointer
        let action = do
                x0 <- use rectX
                y0 <- use rectY
                rectWidth  .= x-x0
                rectHeight .= y-y0
            r1 = execState action r
        setSelection (Just r1)
        draw

    onEvent e = do
        overlap <- isActive Overlap
        if overlap
            then overlapMode e
            else collisionMode e

    overlapMode (Hold r (x,y))     = onHoldOverlapMode r x y
    overlapMode (Resize r (x,y) a) = onResizeOverlapMode r x y a

    collisionMode (Hold r (x,y))     = onHoldCollisionMode r x y
    collisionMode (Resize r (x,y) a) = onResizeCollisionMode r x y a

    onHoldOverlapMode r x0 y0 = do
        (x,y) <- getPointer
        let r1 = translateRect (x-x0) (y-y0) r
        setEvent $ Just $ Hold r1 (x,y)
        draw

    onHoldCollisionMode r x0 y0 = do
        cOpt <- getCollision
        case cOpt of
            Nothing -> onHoldNoPrevCollision r x0 y0
            Just c  -> onHoldPrevCollision r x0 y0 c

    onHoldNoPrevCollision r x0 y0 = do
        (x,y) <- getPointer
        ts    <- getRects
        case intersection ts r of
            Nothing     -> onHoldOverlapMode r x0 y0
            Just (t, d) -> do
                let delta        = collisionDelta d r t
                    r1           = adaptRect d delta r
                    (x1,y1)      = adaptPos d delta x y
                    (tmin, tmax) = rectRange d t
                setEvent $ Just $ Hold r1 (x1,y1)
                setCollision $ Just $ (x0,y0,delta,tmin,tmax,d)
                draw

    onHoldPrevCollision r x1 y1 (x0,y0,delta,tmin,tmax,d) = do
        (x,y) <- getPointer
        let collides  = rangeCollides d tmin tmax r
            diff      = diffPos d delta (x,y) (x0,y0)
            (x1',y1') = adaptPosDefault d delta (x,y) (x1,y1)
            r1        = oppositeTranslate d (x,y) (x1,y1) r
            (x2,y2)   = updateHoldPos d (x,y) (x0,y0)
            catchUp   = diff <= 0
        draw
        if not catchUp && collides
            then do
            setEvent $ Just $ Hold r1 (x2,y2)
            else do
            setCollision Nothing
            let (r2, hstate) = correctRect d (x1',y1') r
            setEvent $ Just $ Hold r2 hstate

    onResizeOverlapMode r x0 y0 a = do
        (x,y) <- getPointer
        let r1 = resizeRect (x-x0) (y-y0) a r
        setEvent $ Just $ Resize r1 (x,y) a
        draw

    onResizeCollisionMode r x0 y0 a = onResizeOverlapMode r x0 y0 a

adaptPos :: Direction
         -> Double
         -> Double
         -> Double
         -> (Double, Double)
adaptPos NORTH delta x y = (x, y-delta)
adaptPos SOUTH delta x y = (x, y-delta)
adaptPos WEST  delta x y = (x-delta, y)
adaptPos EAST  delta x y = (x-delta, y)

adaptPosDefault :: Direction
                -> Double
                -> (Double, Double)
                -> (Double, Double)
                -> (Double, Double)
adaptPosDefault NORTH delta (dx,_) (_,y) = (dx, y-delta)
adaptPosDefault SOUTH delta (dx,_) (_,y) = (dx, y-delta)
adaptPosDefault WEST  delta (_,dy) (x,_) = (x-delta, dy)
adaptPosDefault EAST  delta (_,dy) (x,_) = (x-delta, dy)

adaptRect :: Direction -> Double -> Rect -> Rect
adaptRect NORTH delta r = r & rectY -~ delta
adaptRect SOUTH delta r = r & rectY -~ delta
adaptRect WEST  delta r = r & rectX -~ delta
adaptRect EAST  delta r = r & rectX -~ delta

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

diffPos :: Direction -> Double -> (Double, Double) -> (Double, Double) -> Double
diffPos NORTH delta (_,y1) (_,y0) = y1 - y0 + delta
diffPos SOUTH delta (_,y1) (_,y0) = negate (y1 - y0 + delta)
diffPos WEST  delta (x1,_) (x0,_) = x1 - x0 + delta
diffPos EAST  delta (x1,_) (x0,_) = negate (x1 - x0 + delta)

oppositeTranslate :: Direction
                  -> (Double, Double)
                  -> (Double, Double)
                  -> Rect
                  -> Rect
oppositeTranslate NORTH (x1,_) (x0,_) = translateRectX (x1-x0)
oppositeTranslate SOUTH (x1,_) (x0,_) = translateRectX (x1-x0)
oppositeTranslate WEST  (_,y1) (_,y0) = translateRectY (y1-y0)
oppositeTranslate EAST  (_,y1) (_,y0) = translateRectY (y1-y0)

updateHoldPos :: Direction
              -> (Double, Double)
              -> (Double, Double)
              -> (Double, Double)
updateHoldPos NORTH (x1,_) (_,y0) = (x1,y0)
updateHoldPos SOUTH (x1,_) (_,y0) = (x1,y0)
updateHoldPos WEST (_,y1) (x0,_)  = (x0,y1)
updateHoldPos EAST (_,y1) (x0,_)  = (x0,y1)

correctRect :: Direction -> (Double, Double) -> Rect -> (Rect, (Double, Double))
correctRect NORTH (x,y) r = (translateRectY (negate 1) r, (x, y-1))
correctRect SOUTH (x,y) r = (translateRectY 1 r, (x, y+1))
correctRect WEST  (x,y) r = (translateRectX (negate 1) r, (x-1, y))
correctRect EAST  (x,y) r = (translateRectX 1 r, (x+1, y))

onPress :: DhekProgram ()
onPress = compile $ do
    (x,y) <- getPointer
    oOpt  <- getOverRect
    aOpt  <- getOverArea
    let newSel = rectNew x y 0 0
        onEvent aOpt r = do
            let evt = maybe (Hold r (x,y)) (Resize r (x,y)) aOpt
            setEvent (Just evt)
            detachRect r

    maybe (setSelection (Just newSel)) (onEvent aOpt) oOpt
    draw

onRelease :: DhekProgram ()
onRelease = compile $ do
    sOpt <- getSelection
    eOpt <- getEvent
    traverse_ update eOpt
    traverse_ insert sOpt
    when (isJust eOpt || isJust sOpt) draw
  where
    update e =
        let r0 = case e of
                Hold x _     -> x
                Resize x _ _ -> x
            r  = normalize r0 in
        do attachRect r
           setSelected (Just r)
           setEvent Nothing
           setCollision Nothing

    insert r0 =
        let r1 = normalize r0
            w  = r1 ^. rectWidth
            h  = r1 ^. rectHeight in
        do when (w*h >= 30) $ do
               id <- freshId
               let r2 =  r1 & rectId   .~ id
                            & rectName %~ (++ show id)
               addRect r2
               setSelected (Just r2)
           setSelection Nothing

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

intersection :: [Rect] -> Rect -> Maybe (Rect, Direction)
intersection rs l = getFirst $ foldMap (First . go) rs
  where
    go r = do
        dir <- rectIntersect l r
        return (r, dir)

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

eventCursor :: BoardEvent -> CursorType
eventCursor (Hold _ _)     = Hand1
eventCursor (Resize _ _ a) = areaCursor a

areaCursor :: Area -> CursorType
areaCursor TOP_LEFT     = TopLeftCorner
areaCursor TOP          = TopSide
areaCursor TOP_RIGHT    = TopRightCorner
areaCursor RIGHT        = RightSide
areaCursor BOTTOM_RIGHT = BottomRightCorner
areaCursor BOTTOM       = BottomSide
areaCursor BOTTOM_LEFT  = BottomLeftCorner
areaCursor LEFT         = LeftSide
