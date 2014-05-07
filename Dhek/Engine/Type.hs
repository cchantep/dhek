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

--------------------------------------------------------------------------------
import Control.Lens
import Graphics.UI.Gtk (CursorType)

--------------------------------------------------------------------------------
import Dhek.Types

--------------------------------------------------------------------------------
-- | Mode Monad
--------------------------------------------------------------------------------
class (Monad m, Applicative m) => ModeMonad m where
    mSelectRectangle   :: Rect -> m ()
    mUnselectRectangle :: m ()
    mGetDrawSelection  :: m (Maybe Rect)
    mSetDrawSelection  :: Maybe Rect -> m ()
    mGetEvent          :: m (Maybe BoardEvent)
    mSetEvent          :: Maybe BoardEvent -> m ()
    mGetCollision      :: m (Maybe Collision)
    mSetCollision      :: Maybe Collision -> m ()
    mNewRectangle      :: Rect -> m ()
    mAttachRectangle   :: Rect -> m ()
    mDetachRectangle   :: Rect -> m ()
    mSetCursor         :: Maybe CursorType -> m ()
    mFreshId           :: m Int

--------------------------------------------------------------------------------
-- | Declarations
--------------------------------------------------------------------------------
type Pos = (Double, Double)
type Width = Double
type Zoom = Double

--------------------------------------------------------------------------------
newtype M a = M (forall m. ModeMonad m => m a)

--------------------------------------------------------------------------------
newtype Mode = Mode (forall a. M a -> EngineState -> IO EngineState)

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
      , _drawCursor    :: !(Maybe CursorType)
      , _drawAddedRect :: !(Maybe Rect)
      , _drawCollision :: !(Maybe Collision)
      , _drawDetached  :: !(Maybe Rect)
      , _drawAttached  :: !(Maybe Rect)
      , _drawNewRect   :: !(Maybe Rect)
      , _drawFreshId   :: !Int
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
    , _engineMode      :: Mode
    }

--------------------------------------------------------------------------------
data EngineEnv = EngineEnv
    { _enginePrevX     :: {-# UNPACK #-} !Double
    , _enginePrevY     :: {-# UNPACK #-} !Double
    , _enginePageCount :: {-# UNPACK #-} !Int
    , _engineFilename  :: !String
    , _engineRects     :: ![Rect]
    , _engineOverRect  :: !(Maybe Rect)
    , _engineOverArea  :: !(Maybe Area)
    }

--------------------------------------------------------------------------------
-- | Constructors
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
    mSelectRectangle   = selectRectangle
    mUnselectRectangle = unselectRectangle
    mGetDrawSelection  = getDrawSelection
    mSetDrawSelection  = setDrawSelection
    mGetEvent          = getEvent
    mSetEvent          = setEvent
    mGetCollision      = getCollision
    mSetCollision      = setCollision
    mNewRectangle      = newRectangle
    mAttachRectangle   = attachRectangle
    mDetachRectangle   = detachRectangle
    mSetCursor         = setCursor
    mFreshId           = freshId

--------------------------------------------------------------------------------
-- | Mode Run
--------------------------------------------------------------------------------
runM :: ModeMonad m => M a -> m a
runM (M m) = m

--------------------------------------------------------------------------------
runMode :: Mode -> EngineState -> M a -> IO EngineState
runMode (Mode k) s m = k m s

--------------------------------------------------------------------------------
-- | Mode API
--------------------------------------------------------------------------------
selectRectangle :: Rect -> M ()
selectRectangle r = M $ mSelectRectangle r

--------------------------------------------------------------------------------
unselectRectangle :: M ()
unselectRectangle = M mUnselectRectangle

--------------------------------------------------------------------------------
getDrawSelection :: M (Maybe Rect)
getDrawSelection = M mGetDrawSelection

--------------------------------------------------------------------------------
setDrawSelection :: Maybe Rect -> M ()
setDrawSelection s = M $ mSetDrawSelection s

--------------------------------------------------------------------------------
getEvent :: M (Maybe BoardEvent)
getEvent = M mGetEvent

--------------------------------------------------------------------------------
setEvent :: Maybe BoardEvent -> M ()
setEvent e = M $ mSetEvent e

--------------------------------------------------------------------------------
getCollision :: M (Maybe Collision)
getCollision = M mGetCollision

--------------------------------------------------------------------------------
setCollision :: Maybe Collision -> M ()
setCollision c = M $ mSetCollision c

--------------------------------------------------------------------------------
newRectangle :: Rect -> M ()
newRectangle r = M $ mNewRectangle r

--------------------------------------------------------------------------------
attachRectangle :: Rect -> M ()
attachRectangle r = M $ mAttachRectangle r

--------------------------------------------------------------------------------
detachRectangle :: Rect -> M ()
detachRectangle r = M $ mDetachRectangle r

--------------------------------------------------------------------------------
setCursor :: Maybe CursorType -> M ()
setCursor c = M $ mSetCursor c

--------------------------------------------------------------------------------
freshId :: M Int
freshId = M $ mFreshId
