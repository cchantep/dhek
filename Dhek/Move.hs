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

import Dhek.Engine
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
               handCursor <$ oOpt
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

    onHoldCollisionMode r x0 y0 = onHoldOverlapMode r x0 y0

    onResizeOverlapMode r x0 y0 a = do
        (x,y) <- getPointer
        let r1 = resizeRect (x-x0) (y-y0) a r
        setEvent $ Just $ Resize r1 (x,y) a
        draw

    onResizeCollisionMode r x0 y0 a = onResizeOverlapMode r x0 y0 a

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
