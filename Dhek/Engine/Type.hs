{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Engine.Type
--
-- Engine type declarations
--------------------------------------------------------------------------------
module Dhek.Engine.Type where

--------------------------------------------------------------------------------
import Control.Applicative

--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.State
import qualified Data.IntMap as I
import           Graphics.UI.Gtk (CursorType, Modifier)

--------------------------------------------------------------------------------
import Dhek.Engine.Instr
import Dhek.Types

--------------------------------------------------------------------------------
-- | Mode Monad
--------------------------------------------------------------------------------
class (Monad m, Applicative m) => ModeMonad m where
    mMove       :: DrawEnv -> m ()
    mPress      :: DrawEnv -> m ()
    mRelease    :: DrawEnv -> m ()
    mDrawing    :: PageItem -> Ratio -> m ()
    mKeyPress   :: KbEnv -> m ()
    mKeyRelease :: KbEnv -> m ()

--------------------------------------------------------------------------------
-- | Declarations
--------------------------------------------------------------------------------
type Pos   = (Double, Double)
type Ratio = Double
type Width = Double
type Zoom  = Double

--------------------------------------------------------------------------------
data DhekMode
    = DhekNormal
    | DhekDuplication
    | DhekSelection

--------------------------------------------------------------------------------
newtype M a = M (forall m. ModeMonad m => m a)

--------------------------------------------------------------------------------
newtype Mode = Mode (forall a. M a -> EngineState -> IO EngineState)

--------------------------------------------------------------------------------
-- | We expect from a cleanup handler to handle @EngineState@ state and
--   IO actions
type EngineCtx m = (MonadIO m, MonadState EngineState m)

--------------------------------------------------------------------------------
-- | Holds a Engine mode and a cleanup handler. @ModeManager@ manages anything
--   related to a @Mode@ lifecycle
data ModeManager
    = ModeManager
      { mgrMode    :: Mode
      , mgrCleanup :: forall m. EngineCtx m => m ()
      }

--------------------------------------------------------------------------------
data DrawEnv
    = DrawEnv
      { drawOverlap  :: Bool             -- ^ Rectangle overlap
      , drawPointer  :: (Double, Double) -- ^ (x, y) pointer position
      , drawRects    :: [Rect]           -- ^ Page rectangle
      , drawRatio    :: Double           -- ^ Page ratio
      , drawModifier :: [Modifier]
      }

--------------------------------------------------------------------------------
data KbEnv
    = KbEnv
      { kbKeyName  :: String
      , kbModifier :: [Modifier]
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
      , _drawCurGuide  :: !(Maybe Guide)
      , _drawOverGuide :: !(Maybe Guide)
      }

--------------------------------------------------------------------------------
data EngineState = EngineState
    { _engineCurPage    :: {-# UNPACK #-} !Int
    , _engineCurZoom    :: {-# UNPACK #-} !Int
    , _engineRectId     :: {-# UNPACK #-} !Int
    , _engineOverlap    :: !Bool
    , _engineDraw       :: !Bool
    , _enginePropLabel  :: !String
    , _enginePropType   :: !(Maybe String)
    , _enginePrevPos    :: !(Double, Double)
    , _engineBoards     :: !Boards
    , _engineDrawState  :: !DrawState
    , _engineBaseWidth  :: !Int
    , _engineThick      :: !Double
    , _engineEventStack :: ![Event]
    }

--------------------------------------------------------------------------------
data EngineEnv
    = EngineEnv
      { _engineFilename :: !String }

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
                        , _drawCurGuide  = Nothing
                        , _drawOverGuide = Nothing
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
    mMove       = move
    mPress      = press
    mRelease    = release
    mDrawing    = drawing
    mKeyPress   = keyPress
    mKeyRelease = keyRelease

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
release :: DrawEnv -> M ()
release e = M $ mRelease e

--------------------------------------------------------------------------------
drawing :: PageItem -> Ratio -> M ()
drawing p r = M $ mDrawing p r

--------------------------------------------------------------------------------
keyPress :: KbEnv -> M ()
keyPress e = M $ mKeyPress e

--------------------------------------------------------------------------------
keyRelease :: KbEnv -> M ()
keyRelease e = M $ mKeyRelease e

--------------------------------------------------------------------------------
-- | Helpers
--------------------------------------------------------------------------------
engineStateGetRects :: MonadState EngineState m => m [Rect]
engineStateGetRects = do
    pid <- use engineCurPage
    use $ engineBoards.boardsMap.at pid.traverse.boardRects.to I.elems

--------------------------------------------------------------------------------
engineStateSetRects :: MonadState EngineState m => [Rect] -> m ()
engineStateSetRects rs = do
    pid <- use engineCurPage
    forM_ rs $ \r -> do
        let rid = r ^. rectId
        engineBoards.boardsMap.at pid.traverse.boardRects.at rid ?= r

--------------------------------------------------------------------------------
engineStateSetRect :: MonadState EngineState m => Rect -> m ()
engineStateSetRect r
    = do pid <- use engineCurPage
         let rid = r ^. rectId
         engineBoards.boardsMap.at pid.traverse.boardRects.at rid ?= r

--------------------------------------------------------------------------------
engineStateGetGuides :: MonadState EngineState m => m [Guide]
engineStateGetGuides
    = do pid <- use engineCurPage
         use $ engineBoards.boardsMap.at pid.traverse.boardGuides

--------------------------------------------------------------------------------
engineStateSetGuides :: MonadState EngineState m => [Guide] -> m ()
engineStateSetGuides gs
    = do pid <- use engineCurPage
         engineBoards.boardsMap.at pid.traverse.boardGuides .= gs
