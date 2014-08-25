{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Cartesian
--
--------------------------------------------------------------------------------
module Dhek.Cartesian where

--------------------------------------------------------------------------------
import Control.Lens hiding (from, to)

--------------------------------------------------------------------------------
data Point2D
    = Point2D
      { _pointX :: !Double
      , _pointY :: !Double
      }
    deriving (Eq, Show)

--------------------------------------------------------------------------------
data Vector2D
    = Vector2D
      { _vectorFrom :: !Point2D
      , _vectorTo   :: !Point2D
      }
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------
makeLenses ''Point2D
makeLenses ''Vector2D

--------------------------------------------------------------------------------
vector2D :: Point2D -> Point2D -> Vector2D
vector2D = Vector2D

--------------------------------------------------------------------------------
vector2DPts :: Double -> Double -> Double -> Double -> Vector2D
vector2DPts fx fy tx ty = vector2D (point2D fx fy) (point2D tx ty)

--------------------------------------------------------------------------------
point2D :: Double -> Double -> Point2D
point2D = Point2D

--------------------------------------------------------------------------------
swapVector2D :: Vector2D -> Vector2D
swapVector2D v = newV
  where
    newFrom = v ^. vectorTo
    newTo   = v ^. vectorFrom
    newV    = v & vectorFrom .~ newFrom
                & vectorTo   .~ newTo

--------------------------------------------------------------------------------
swapXVector2D :: Vector2D -> Vector2D
swapXVector2D v = newV
  where
    newFromX = v ^. vectorTo.pointX
    newToX   = v ^. vectorFrom.pointX
    newV     = v & vectorFrom.pointX .~ newFromX
                 & vectorTo.pointX   .~ newToX

--------------------------------------------------------------------------------
swapYVector2D :: Vector2D -> Vector2D
swapYVector2D v = newV
  where
    newFromY = v ^. vectorTo.pointY
    newToY   = v ^. vectorFrom.pointY
    newV     = v & vectorFrom.pointY .~ newFromY
                 & vectorTo.pointY   .~ newToY

--------------------------------------------------------------------------------
vector2DBackward :: Vector2D -> Bool
vector2DBackward v = tx - fx < 0
  where
    fx = v ^. vectorFrom.pointX
    tx = v ^. vectorTo.pointX

--------------------------------------------------------------------------------
vector2DUpward :: Vector2D -> Bool
vector2DUpward v = ty - fy < 0
  where
    fy = v ^. vectorFrom.pointY
    ty = v ^. vectorTo.pointY

--------------------------------------------------------------------------------
vector2DWidth :: Vector2D -> Double
vector2DWidth v = abs $ vector2DDeltaX v

--------------------------------------------------------------------------------
vector2DHeight :: Vector2D -> Double
vector2DHeight v = abs $ vector2DDeltaY v

--------------------------------------------------------------------------------
vector2DDeltaX :: Vector2D -> Double
vector2DDeltaX v = tx - fx
  where
    fx = v ^. vectorFrom.pointX
    tx = v ^. vectorTo.pointX

--------------------------------------------------------------------------------
vector2DDeltaY :: Vector2D -> Double
vector2DDeltaY v = ty-fy
  where
    fy = v ^. vectorFrom.pointY
    ty = v ^. vectorTo.pointY
