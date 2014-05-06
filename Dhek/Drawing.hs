{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Drawing
--
-- Draw instruction declaration
--------------------------------------------------------------------------------
module Dhek.Drawing where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad
import Data.Foldable (find)
import Data.Traversable (for)

--------------------------------------------------------------------------------
import           Control.Lens hiding (Zoom)
import           Control.Monad.Reader
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
import Dhek.Engine.Type
import Dhek.Types

--------------------------------------------------------------------------------
type Drawing a = DrawOptions -> M a

--------------------------------------------------------------------------------
execDrawing :: DrawOptions -> Drawing a -> M a
execDrawing opts k = k opts

--------------------------------------------------------------------------------
getOverRect :: Drawing (Maybe Rect)
getOverRect o = do
    let (x,y) = drawPointer o
        rs    = drawRects o
    return $ find (isOver 1.0 x y) rs

--------------------------------------------------------------------------------
getOverArea :: Drawing (Maybe Area)
getOverArea o = do
    let ratio = drawRatio o
        (x,y) = drawPointer o

    rOpt  <- getOverRect o
    return $ rOpt >>= \r ->
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
