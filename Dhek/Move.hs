module Dhek.Move where

import Control.Applicative ((<|>), (<$))
import Control.Lens (use, (.=), (%=), (?=), (+=), (-=), (<%=), (^.), (&), (.~), (%~))
import Control.Monad ((<=<), when)
import Control.Monad.Reader (ask)
import Control.Monad.State (execState)

import Data.Foldable (traverse_)
import Data.Maybe (isJust)
import Data.Traversable (traverse)

import Dhek.Engine
import Dhek.Instr
import Dhek.Types hiding (addRect)

onMove :: DhekProgram ()
onMove = compile $ do
    (x,y) <- getPointer
    oOpt  <- getOverRect
    aOpt  <- getOverArea
    sOpt  <- getSelection
    eOpt  <- getEvent
    let  selection = do
             x0 <- use rectX
             y0 <- use rectY
             rectWidth  .= x - x0
             rectHeight .= y - y0

         event (Hold r (x0,y0))     =
             Hold (translateRect (x-x0) (y-y0) r) (x,y)
         event (Resize r (x0,y0) a) =
             Resize (resizeRect (x-x0) (y-y0) a r) (x,y) a

         cOpt = fmap eventCursor eOpt <|>
                fmap areaCursor aOpt  <|>
                handCursor <$ oOpt

    setSelection $ fmap (execState selection) sOpt
    setEvent     $ fmap event eOpt
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
