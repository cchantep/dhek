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
import Dhek.Types

onMove :: EngineCallback Move
onMove (Move x y) = do
    env <- ask
    let oOpt = _engineOverRect env
        aOpt = _engineOverArea env
    sOpt <- engineSelection <%= fmap (execState selection)
    eOpt <- engineEvent     <%= fmap event
    cursor eOpt aOpt oOpt
    engineDraw .= (isJust sOpt || isJust eOpt)
  where
    selection = do
        x0 <- use rectX
        y0 <- use rectY
        rectWidth  .= x - x0
        rectHeight .= y - y0

    event (Hold r (x0,y0))     = Hold (translateRect (x-x0) (y-y0) r) (x,y)
    event (Resize r (x0,y0) a) = Resize (resizeRect (x-x0) (y-y0) a r) (x,y) a

    cursor eOpt aOpt oOpt =
        let cOpt = fmap eventCursor eOpt <|>
                   fmap areaCursor aOpt  <|>
                   handCursor <$ oOpt
        in
        engineCursor .= cOpt

onPress :: EngineCallback Press
onPress (Press x y) = do
    env <- ask
    let oOpt   = _engineOverRect env
        aOpt   = _engineOverArea env
        newSel = rectNew x y 0 0
    maybe (engineSelection ?= newSel) (onEvent aOpt) oOpt
    engineDraw .= True
  where
    onEvent aOpt r = do
        let evt = maybe (Hold r (x,y)) (Resize r (x,y)) aOpt
        engineEvent   ?= evt
        engineRemRect ?= r

onRelease :: EngineCallback Release
onRelease _ = do
    eOpt <- use engineEvent
    sOpt <- use engineSelection
    traverse_ update eOpt
    traverse_ insert sOpt
    engineDraw .= (isJust eOpt || isJust sOpt)
  where
    update e =
        let r0 = case e of
                Hold x _     -> x
                Resize x _ _ -> x
            r  = normalize r0 in
        do engineAddedRect ?= r
           engineSelected  ?= r
           engineEvent     .= Nothing


    insert r0 =
        let w  = r0 ^. rectWidth
            h  = r0 ^. rectHeight
            r1 = normalize r0 in
        do when (w*h >= 30) $ do
               id <- freshId
               let r2 =  r1 & rectId   .~ id
                            & rectName %~ (++ show id)
               engineAddedRect ?= r2
               engineSelected  ?= r2
           engineSelection .= Nothing

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
