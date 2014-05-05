{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
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
import           Control.Monad.State
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
import Dhek.Types

--------------------------------------------------------------------------------
newtype Drawing a
    = Drawing { runDrawing :: DrawOptions -> DrawState -> (a, DrawState ) }

--------------------------------------------------------------------------------
instance Functor Drawing where
    fmap f (Drawing k)
        = Drawing $ \e s -> let (a, s') = k e s in (f a, s')

--------------------------------------------------------------------------------
instance Applicative Drawing where
    pure  = return
    (<*>) = ap

--------------------------------------------------------------------------------
instance Monad Drawing where
    return a = Drawing $ \_ s -> (a, s)

    Drawing k >>= f
        = Drawing $ \e s ->
            let (a, s') = k e s in runDrawing (f a) e s'

--------------------------------------------------------------------------------
instance MonadReader DrawOptions Drawing where
    ask = Drawing $ \e s -> (e, s)

    local f m = Drawing $ \e s -> runDrawing m (f e) s

--------------------------------------------------------------------------------
instance MonadState DrawState Drawing where
    state k = Drawing $ \e -> k

--------------------------------------------------------------------------------
data DrawOptions
    = DrawOptions
      { drawOverlap :: Bool             -- ^ Rectangle overlap
      , drawPointer :: (Double, Double) -- ^ (x, y) pointer position
      , drawRects   :: [Rect]           -- ^ Page rectangle
      , drawRatio   :: Double           -- ^ Page ratio
      }

--------------------------------------------------------------------------------
data Collision
    = Collision
      { colDelta     :: !Double
      , colRange     :: !(Double, Double)
      , colPrevPos   :: !(Double, Double)
      , colDirection :: !Direction
      }

--------------------------------------------------------------------------------
data DrawState
    = DrawState
      { _drawEvent     :: !(Maybe BoardEvent)
      , _drawSelection :: !(Maybe Rect)
      , _drawSelected  :: !(Maybe Rect)
      , _drawCursor    :: !(Maybe Gtk.CursorType)
      , _drawAddedRect :: !(Maybe Rect)
      , _drawCollision :: !(Maybe Collision)
      , _drawDetached  :: !(Maybe Rect)
      , _drawAttached  :: !(Maybe Rect)
      , _drawNewRect   :: !(Maybe Rect)
      , _drawFreshId   :: !Int
      }

--------------------------------------------------------------------------------
type Pos = (Double, Double)
type Width = Double
type Zoom = Double

--------------------------------------------------------------------------------
class DrawInterpreter a where
    drawInterpret :: a -> Pos -> Drawing b -> IO ()

--------------------------------------------------------------------------------
makeLenses ''DrawState

--------------------------------------------------------------------------------
drawStateNew :: DrawState
drawStateNew = DrawState{ _drawEvent     = Nothing
                        , _drawSelection = Nothing
                        , _drawSelected  = Nothing
                        , _drawCursor    = Nothing
                        , _drawAddedRect = Nothing
                        , _drawCollision = Nothing
                        , _drawDetached  = Nothing
                        , _drawAttached  = Nothing
                        , _drawNewRect   = Nothing
                        , _drawFreshId   = 0
                        }

--------------------------------------------------------------------------------
drawStateWith :: State DrawState a -> DrawState
drawStateWith action = execState action drawStateNew

--------------------------------------------------------------------------------
evalDrawing :: DrawOptions -> DrawState -> Drawing a -> DrawState
evalDrawing opts st d = snd $ runDrawing d opts st

--------------------------------------------------------------------------------
overlapEnabled :: Drawing Bool
overlapEnabled = asks drawOverlap

--------------------------------------------------------------------------------
getRatio :: Drawing Double
getRatio = asks drawRatio

--------------------------------------------------------------------------------
getPointer :: Drawing (Double, Double)
getPointer = asks drawPointer

--------------------------------------------------------------------------------
rectangles :: Drawing [Rect]
rectangles = asks drawRects

--------------------------------------------------------------------------------
getOverRect :: Drawing (Maybe Rect)
getOverRect = do
    (x,y) <- getPointer
    rs    <- asks drawRects
    return $ find (isOver 1.0 x y) rs

--------------------------------------------------------------------------------
getOverArea :: Drawing (Maybe Area)
getOverArea = do
    ratio <- getRatio
    rOpt  <- getOverRect
    (x,y) <- getPointer
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
