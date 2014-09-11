--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Mode.Effect.Magnetic
--
--
--------------------------------------------------------------------------------
module Dhek.Mode.Effect.Magnetic
    ( Attract
    , Line
    , magneticAttraction
    , magneticMove
    , magneticDraw
    , magneticResize
    , horizontalLine
    , verticalLine
    , projectMagnetic
    ) where

--------------------------------------------------------------------------------
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (First(..))

--------------------------------------------------------------------------------
import Control.Lens

--------------------------------------------------------------------------------
import Dhek.Cartesian
import Dhek.Geometry
import Dhek.Types

--------------------------------------------------------------------------------
data Attract
    = Attract
      { attractLineType :: !LineType
      , attractForce    :: !Force
      , attractValue    :: !Double
      , attractLocation :: !Location
      } deriving Show

--------------------------------------------------------------------------------
data Location
    = After
    | Before
    deriving Show

--------------------------------------------------------------------------------
data LineType
    = Horizontal
    | Vertical
    deriving Show

--------------------------------------------------------------------------------
data Line = Line !LineType !Double deriving Show

--------------------------------------------------------------------------------
type Force = Double

--------------------------------------------------------------------------------
data Magnetic
    = MagneticMove Rect Vector2D
    | MagneticResize Rect Area Vector2D
    | MagneticDraw Vector2D

--------------------------------------------------------------------------------
attract :: LineType
        -> Force
        -> Double
        -> Location
        -> Attract
attract typ force value loc
    = Attract
      { attractLineType = typ
      , attractForce    = force
      , attractValue    = value
      , attractLocation = loc
      }

--------------------------------------------------------------------------------
magneticAttraction :: Foldable f => Magnetic -> Force -> f Line -> Maybe Attract
magneticAttraction (MagneticMove r v) f xs     = attractMove f r v xs
magneticAttraction (MagneticResize r a v) f xs = attractResize f r a v xs
magneticAttraction (MagneticDraw v) f xs       = attractDraw f v xs

--------------------------------------------------------------------------------
attractMove :: Foldable f
            => Force
            -> Rect
            -> Vector2D
            -> f Line
            -> Maybe Attract
attractMove force r vect ls = getFirst $ foldMap (First . go) ls
  where
    projRect = moveRect vect r

    px = projRect ^. rectX
    py = projRect ^. rectY
    pw = projRect ^. rectWidth
    ph = projRect ^. rectHeight

    go (Line Vertical v)
        = case () of
        _ | px+pw >= v-force && px+pw <= v
            -> Just $ attract Vertical force v Before
          | px <= v+force && px >= v
            -> Just $ attract Vertical force v After
          | otherwise
            -> Nothing
    go (Line Horizontal v)
        = case () of
        _ | py+ph >= v-force && py+ph <= v
            -> Just $ attract Horizontal force v Before
          | py <= v+force && py >= v
            -> Just $ attract Horizontal force v After
          | otherwise
            -> Nothing

--------------------------------------------------------------------------------
projectMagnetic :: Magnetic -> Attract -> Rect
projectMagnetic (MagneticMove r v) a = projectMagneticMove r v a
projectMagnetic _ _ = error "projectMagnetic not implemented yet"

--------------------------------------------------------------------------------
projectMagneticMove :: Rect -> Vector2D -> Attract -> Rect
projectMagneticMove r v a = moveRect corVect projRect
  where
    projRect = moveRect v r

    px = projRect ^. rectX
    py = projRect ^. rectY
    pw = projRect ^. rectWidth
    ph = projRect ^. rectHeight

    ltype = attractLineType a
    value = attractValue a
    loc   = attractLocation a

    corVect
        = case ltype of
              Vertical
                  -> case loc of
                         After
                             -> vector2DPts 0 0 (value-px) 0
                         Before
                             -> vector2DPts 0 0 (value-(px+pw)) 0
              Horizontal
                  -> case loc of
                         After
                             -> vector2DPts 0 0 0 (value-py)
                         Before
                             -> vector2DPts 0 0 0 (value-(py+ph))


--------------------------------------------------------------------------------
attractResize :: Foldable f
              => Force
              -> Rect
              -> Area
              -> Vector2D
              -> f Line
              -> Maybe Attract
attractResize _ _ _ _ _ = Nothing

--------------------------------------------------------------------------------
attractDraw :: Foldable f => Force -> Vector2D -> f Line -> Maybe Attract
attractDraw _ _ _ = Nothing

--------------------------------------------------------------------------------
verticalLine :: Double -> Line
verticalLine v = Line Vertical v

--------------------------------------------------------------------------------
horizontalLine :: Double -> Line
horizontalLine v = Line Horizontal v

--------------------------------------------------------------------------------
magneticMove :: Rect -> Vector2D -> Magnetic
magneticMove r v = MagneticMove r v

--------------------------------------------------------------------------------
magneticResize :: Rect -> Area -> Vector2D -> Magnetic
magneticResize r a v = MagneticResize r a v

--------------------------------------------------------------------------------
magneticDraw :: Vector2D -> Magnetic
magneticDraw v = MagneticDraw v
