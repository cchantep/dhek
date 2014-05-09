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
import           Control.Monad.RWS.Strict
import           Control.Monad.Trans
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
import Dhek.Engine.Type
import Dhek.Geometry
import Dhek.GUI
import Dhek.GUI.Action
import Dhek.Types

--------------------------------------------------------------------------------
newtype DuplicateMode a
    = DuplicateMode (RWST GUI () EngineState IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader GUI
             , MonadState EngineState
             , MonadIO
             )

--------------------------------------------------------------------------------
instance ModeMonad DuplicateMode where
    mMove opts = do
        let oOpt = getOverRect opts

        eOpt <- use $ engineDrawState.drawEvent

        engineDrawState.drawOverRect .= oOpt

        -- We only handle move without caring about overlap
        for_ eOpt $ \e -> do
            let pos@(x,y) = drawPointer opts
            case e of
                Hold r ppos ->
                    engineDrawState.drawEvent ?=
                        Hold (updateHoldRect ppos pos r) (x,y)

                _ -> return ()

    mPress opts = do
        eOpt <- use $ engineDrawState.drawEvent
        case eOpt of
            Nothing -> for_ (getOverRect opts) $ \r -> do
                rid <- engineDrawState.drawFreshId <+= 1
                let r2    = r & rectId .~ rid
                    (x,y) = drawPointer opts

                engineDrawState.drawEvent ?= Hold r2 (x,y)
            Just (Hold x _) -> do
                rid <- engineDrawState.drawFreshId <+= 1
                let r = normalize x & rectId .~ rid

                -- Add rectangle
                pid <- use engineCurPage
                gui <- ask
                engineBoards.boardsMap.at pid.traverse.boardRects.at rid ?= r
                liftIO $ gtkAddRect r gui

                engineDrawState.drawEvent     .= Nothing
                engineDrawState.drawCollision .= Nothing

    mRelease = return ()

--------------------------------------------------------------------------------
runDuplicate :: GUI -> DuplicateMode a -> EngineState -> IO EngineState
runDuplicate gui (DuplicateMode m) s = do
    (s', _) <- execRWST m gui s
    return s'

--------------------------------------------------------------------------------
duplicateMode :: GUI -> Mode
duplicateMode gui = Mode (runDuplicate gui . runM)
