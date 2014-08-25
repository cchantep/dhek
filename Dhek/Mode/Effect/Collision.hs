--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Mode.Effect.Collision
--
--
--------------------------------------------------------------------------------
module Dhek.Mode.Effect.Collision
       ( Collide
       , Collision
       , collisionCollide
       , collisionMove
       , projectCollision
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
data Direction
    = NORTH
    | EAST
    | SOUTH
    | WEST

--------------------------------------------------------------------------------
data Collide
    = Collide
      { colDirection :: !Direction
      , colTarget    :: !Rect
      }

--------------------------------------------------------------------------------
data Collision
    = CollisionMove Rect Vector2D

--------------------------------------------------------------------------------
collisionCollide :: Foldable f => Collision -> f Rect -> Maybe Collide
collisionCollide (CollisionMove r v) rs = collisionCollideMove r v rs

--------------------------------------------------------------------------------
collisionCollideMove :: Foldable f
                     => Rect
                     -> Vector2D
                     -> f Rect
                     -> Maybe Collide
collisionCollideMove r v rs
    = fmap go $ intersection rs projRect
  where
    projRect = moveRect v r

    go (r', d) = mkCollide r' d

--------------------------------------------------------------------------------
projectCollision :: Collision -> Collide -> Rect
projectCollision (CollisionMove r v) c = projectCollisionMove r v c

--------------------------------------------------------------------------------
projectCollisionMove :: Rect -> Vector2D -> Collide -> Rect
projectCollisionMove r v c
    = moveRect corVect projRect
  where
    projRect = moveRect v r
    dir      = colDirection c
    target   = colTarget c

    px = projRect ^. rectX
    py = projRect ^. rectY
    ph = projRect ^. rectHeight
    pw = projRect ^. rectWidth

    tx = target ^. rectX
    ty = target ^. rectY
    th = target ^. rectHeight
    tw = target ^. rectWidth

    corVect =
        case dir of
            WEST  -> vector2DPts 0 0 ((tx-1)-(px+pw)) 0
            NORTH -> vector2DPts 0 0 0 ((ty-1)-(py+ph))
            EAST  -> vector2DPts 0 0 ((tx+tw+1)-(px)) 0
            SOUTH -> vector2DPts 0 0 0 ((ty+th+1)-(py))

--------------------------------------------------------------------------------
mkCollide :: Rect -> Direction -> Collide
mkCollide target direction
    = Collide
      { colDirection = direction
      , colTarget    = target
      }

--------------------------------------------------------------------------------
intersection :: Foldable f => f Rect -> Rect -> Maybe (Rect, Direction)
intersection rs l = getFirst $ foldMap (First . go) rs
  where
    go r = do dir <- rectIntersect l r
              return (r, dir)

--------------------------------------------------------------------------------
-- | Using Minkowski Sum
rectIntersect :: Rect -> Rect -> Maybe Direction
rectIntersect a b
    | not collides = Nothing
    | otherwise =
        if wy > hx
        then if wy > negate hx
             then Just SOUTH
             else Just WEST
        else if wy > negate hx
             then Just EAST
             else Just NORTH
  where
    aw = a ^. rectWidth
    ah = a ^. rectHeight
    ax = a ^. rectX
    ay = a ^. rectY
    acenterx = ax + aw / 2
    acentery = ay + ah / 2

    bw = b ^. rectWidth
    bh = b ^. rectHeight
    bx = b ^. rectX
    by = b ^. rectY
    bcenterx = bx + bw / 2
    bcentery = by + bh / 2

    w  = 0.5 * (aw + bw)
    h  = 0.5 * (ah + bh)
    dx = acenterx - bcenterx
    dy = acentery - bcentery

    collides = abs dx <= w && abs dy <= h

    wy = w * dy
    hx = h * dx

--------------------------------------------------------------------------------
collisionMove :: Rect -> Vector2D -> Collision
collisionMove r v = CollisionMove r v
