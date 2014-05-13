{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Engine.Type
--
-- Engine type declarations
--------------------------------------------------------------------------------
module Dhek.Engine.Type where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Array (Array)

--------------------------------------------------------------------------------
import Control.Lens
import Graphics.UI.Gtk (CursorType)

--------------------------------------------------------------------------------
import Dhek.Types

--------------------------------------------------------------------------------
-- | Mode Monad
--------------------------------------------------------------------------------
class (Monad m, Applicative m) => ModeMonad m where
    mMove    :: DrawEnv -> m ()
    mPress   :: DrawEnv -> m ()
    mRelease :: m ()
    mDrawing :: PageItem -> Ratio -> m ()

--------------------------------------------------------------------------------
-- | Declarations
--------------------------------------------------------------------------------
type Pos   = (Double, Double)
type Ratio = Double
type Width = Double
type Zoom  = Double

--------------------------------------------------------------------------------
newtype M a = M (forall m. ModeMonad m => m a)

--------------------------------------------------------------------------------
newtype Mode = Mode (forall a. M a -> EngineState -> IO EngineState)

--------------------------------------------------------------------------------
data DrawEnv
    = DrawEnv
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
      , _drawCursor    :: !(Maybe CursorType)
      , _drawCollision :: !(Maybe Collision)
      , _drawOverRect  :: !(Maybe Rect)
      , _drawFreshId   :: !Int
      , _drawMultiSel  :: ![Rect]
      }

--------------------------------------------------------------------------------
data EngineState = EngineState
    { _engineCurPage   :: {-# UNPACK #-} !Int
    , _engineCurZoom   :: {-# UNPACK #-} !Int
    , _engineRectId    :: {-# UNPACK #-} !Int
    , _engineOverlap   :: !Bool
    , _engineDraw      :: !Bool
    , _enginePropLabel :: !String
    , _enginePropType  :: !(Maybe String)
    , _enginePrevPos   :: !(Double, Double)
    , _engineBoards    :: !Boards
    , _engineDrawState :: !DrawState
    , _engineMode      :: !Mode
    , _engineBaseWidth :: !Int
    , _engineThick     :: !Double
    }

--------------------------------------------------------------------------------
data EngineEnv = EngineEnv
    { _engineFilename  :: !String
    , _engineRects     :: ![Rect]
    , _engineOverRect  :: !(Maybe Rect)
    , _engineOverArea  :: !(Maybe Area)
    , _engineModes     :: !(Array Int Mode)
    }

--------------------------------------------------------------------------------
-- | Constructors
--------------------------------------------------------------------------------
drawStateNew :: DrawState
drawStateNew = DrawState{ _drawEvent     = Nothing
                        , _drawSelection = Nothing
                        , _drawSelected  = Nothing
                        , _drawCursor    = Nothing
                        , _drawCollision = Nothing
                        , _drawOverRect  = Nothing
                        , _drawFreshId   = 0
                        , _drawMultiSel  = []
                        }

--------------------------------------------------------------------------------
-- | Lenses
--------------------------------------------------------------------------------
makeLenses ''EngineState
makeLenses ''DrawState

--------------------------------------------------------------------------------
-- | Mode instances
--------------------------------------------------------------------------------
instance Functor M where
    fmap f (M m) = M $ fmap f m

--------------------------------------------------------------------------------
instance Applicative M where
    pure a = M $ pure a
    (M f) <*> (M a) = M (f <*> a)

--------------------------------------------------------------------------------
instance Monad M where
    return a = M $ return a

    M m >>= f = M (m >>= \a -> runM (f a))

--------------------------------------------------------------------------------
instance ModeMonad M where
    mMove    = move
    mPress   = press
    mRelease = release
    mDrawing = drawing

--------------------------------------------------------------------------------
-- | Mode Run
--------------------------------------------------------------------------------
runM :: ModeMonad m => M a -> m a
runM (M m) = m

--------------------------------------------------------------------------------
runMode :: Mode -> EngineState -> M a -> IO EngineState
runMode (Mode k) s m = k m s

--------------------------------------------------------------------------------
-- | Mode callback handlers
--------------------------------------------------------------------------------
move :: DrawEnv -> M ()
move e = M $ mMove e

--------------------------------------------------------------------------------
press :: DrawEnv -> M ()
press e = M $ mPress e

--------------------------------------------------------------------------------
release :: M ()
release = M mRelease

drawing :: PageItem -> Ratio -> M ()
drawing p r = M $ mDrawing p r
