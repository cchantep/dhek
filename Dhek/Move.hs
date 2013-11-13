module Dhek.Move where

import Control.Applicative ((<|>), (<$))
import Control.Lens (use, (.=), (%=), (?=), (+=), (-=), (<%=), (^.), (&), (.~), (%~), (-~))
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

onMove :: DhekProgram ()
onMove = compile $ do
    (x,y)   <- getPointer
    oOpt    <- getOverRect
    aOpt    <- getOverArea
    sOpt    <- getSelection
    eOpt    <- getEvent
    lOpt    <- getCollision
    overlap <- isActive Overlap
    let onEvent   = isJust eOpt
        selection = do
             x0 <- use rectX
             y0 <- use rectY
             rectWidth  .= x - x0
             rectHeight .= y - y0

        event (Hold r (x0,y0))     =
            Hold (translateRect (x-x0) (y-y0) r) (x,y)
        event (Resize r (x0,y0) a) =
            Resize (resizeRect (x-x0) (y-y0) a r) (x,y) a

        eventD NORTH (Hold r (x0,y0)) =
            Hold (translateRectX (x-x0) r) (x,y0)
        eventD SOUTH (Hold r (x0,y0)) =
            Hold (translateRectX (x-x0) r) (x,y0)
        eventD WEST (Hold r (x0,y0)) =
            Hold (translateRectY (y-y0) r) (x0,y)
        eventD EAST (Hold r (x0,y0)) =
            Hold (translateRectY (y-y0) r) (x0,y)
        eventD _ e = event e

        cOpt = fmap eventCursor eOpt <|>
               fmap areaCursor aOpt  <|>
               handCursor <$ oOpt

        eOpt2 = fmap event eOpt
        sOpt2 = fmap (execState selection) sOpt

        onCollisionActivated = do
            rs <- getRects
            for_ (eOpt2 >>= eventGetRect) $ \l ->
                for_ (intersection rs l) $ \(r, d) -> do
                    setCollision $ Just (x,y,d)
                    setEventRect (replaceRect d l r)

        prevCollision (x0,y0,d) = do
            rs <- getRects
            let delta =
                    case d of
                        NORTH -> y-y0
                        SOUTH -> y0-y
                        EAST  -> x-x0
                        WEST  -> x0-x

                catchUp  = delta <= 0
                ccOpt    = (intersection rs <=< eventGetRect) =<< eOpt
                collides = isJust ccOpt
                eOpt3    = if catchUp -- || not collides
                           then eOpt2
                           else fmap (eventD d) eOpt
            setEvent eOpt3
            when catchUp (setCollision Nothing)

        noPrevCollision = do
            setEvent eOpt2
            when (onEvent && not overlap) onCollisionActivated

    setSelection sOpt2
    maybe noPrevCollision prevCollision lOpt
    setCursor cOpt
    when (isJust sOpt || isJust eOpt) draw

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

replaceRect :: Direction -> Rect -> Rect -> Rect
replaceRect d l r  = r1
  where
    lx = l ^. rectX
    ly = l ^. rectY
    lw = l ^. rectWidth
    lh = l ^. rectHeight

    r1 =
        case d of
            NORTH -> l & rectY -~ (ly+lh) - (r ^. rectY)
            EAST  -> l & rectX -~ (lx+lw) - (r ^. rectX)
            SOUTH -> l & rectY .~ (r ^. rectY) + (r ^. rectHeight)
            WEST  -> l & rectX .~ (r ^. rectX) + (r ^. rectWidth)

intersection :: [Rect] -> Rect -> Maybe (Rect, Direction)
intersection rs l = getFirst $ foldMap (First . go) rs
  where
    go r = do
        dir <- rectIntersect l r
        return (r, dir)
