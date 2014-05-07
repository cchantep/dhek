{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Mode.Duplicate
--
--------------------------------------------------------------------------------
module Dhek.Mode.Duplicate where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Foldable (for_)
import Data.IORef

--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.RWS
import           Control.Monad.Trans
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
import Dhek.Engine.Type
import Dhek.GUI
import Dhek.GUI.Action
import Dhek.Types

--------------------------------------------------------------------------------
data Input
    = Input
      { prevOverlapSetting :: Bool
      , gui                :: GUI
      }

--------------------------------------------------------------------------------
newtype DuplicateMode a
    = DuplicateMode (RWST Input () EngineState IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Input
             , MonadState EngineState
             , MonadIO
             )

--------------------------------------------------------------------------------
instance ModeMonad DuplicateMode where
    mSelectRectangle r =
        return ()

    mUnselectRectangle =
        return ()

    mGetDrawSelection =
        use $ engineDrawState.drawSelection

    mSetDrawSelection s =
        return ()

    mGetEvent =
        use $ engineDrawState.drawEvent

    mSetEvent opt = do
        prev <- mGetEvent
        maybe init update prev

      where
        init = for_ opt $ \e -> do
            fid  <- mFreshId
            prev <- use engineOverlap

            let (pos, r) = _eventRect e
                r2       = r & rectId   .~ fid
                             & rectName %~ (++ (" dup" ++ show fid))

            engineOverlap             .= True
            engineDrawState.drawEvent ?= Hold r2 pos

        update _ =
            engineDrawState.drawEvent .= opt

    mGetCollision =
        use $ engineDrawState.drawCollision

    mSetCollision c =
        engineDrawState.drawCollision .= Nothing

    mNewRectangle r =
        return ()

    mAttachRectangle r = do
        Input prev gui <- ask
        pid            <- use engineCurPage
        ans            <- use engineOverlap

        let rid = r ^. rectId

        engineOverlap .= not prev
        engineBoards.boardsMap.at pid.traverse.boardRects.at rid ?= r

        liftIO $ gtkAddRect r gui

    mDetachRectangle r =
        return ()

    mSetCursor t =
        return ()

    mFreshId =
        engineDrawState.drawFreshId <+= 1

--------------------------------------------------------------------------------
runDuplicate :: GUI -> DuplicateMode a -> EngineState -> IO EngineState
runDuplicate gui (DuplicateMode m)  s = do
    let prev = s ^. engineOverlap
    (s', _) <- execRWST m (Input prev gui) s
    return s'

--------------------------------------------------------------------------------
duplicateMode :: GUI -> Mode
duplicateMode gui = Mode (runDuplicate gui . runM)

--------------------------------------------------------------------------------
_eventRect :: BoardEvent -> (Pos, Rect)
_eventRect (Hold r pos)     = (pos, r)
_eventRect (Resize r pos _) = (pos, r)
