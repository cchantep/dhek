{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Mode.Normal
--
--------------------------------------------------------------------------------
module Dhek.Mode.Normal where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Traversable

--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.RWS
import           Control.Monad.Trans
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
import Dhek.Drawing
import Dhek.Engine.Type
import Dhek.GUI
import Dhek.GUI.Action
import Dhek.Types

--------------------------------------------------------------------------------
newtype NormalMode a
    = NormalMode (RWST GUI () EngineState IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader GUI
             , MonadState EngineState
             , MonadIO
             )

--------------------------------------------------------------------------------
instance ModeMonad NormalMode where
    mSelectRectangle r = do
        engineDrawState.drawSelected ?= r
        gui <- ask
        liftIO $ gtkSelectRect r gui

    mUnselectRectangle = do
        engineDrawState.drawSelected .= Nothing
        gui <- ask
        liftIO $ gtkUnselect gui

    mGetDrawSelection =
        use $ engineDrawState.drawSelection

    mSetDrawSelection s =
        engineDrawState.drawSelection .= s

    mGetEvent =
        use $ engineDrawState.drawEvent

    mSetEvent e =
        engineDrawState.drawEvent .= e

    mGetCollision =
        use $ engineDrawState.drawCollision

    mSetCollision c =
        engineDrawState.drawCollision .= c

    mNewRectangle r = do
        let rid = r ^. rectId

        gui <- ask
        pid <- use engineCurPage
        engineBoards.boardsMap.at pid.traverse.boardRects.at rid ?= r

        liftIO $ gtkAddRect r gui

    mAttachRectangle r = do
        let rid = r ^. rectId

        pid <- use engineCurPage
        engineBoards.boardsMap.at pid.traverse.boardRects.at rid ?= r

    mDetachRectangle r = do
        let rid = r ^. rectId

        pid <- use engineCurPage
        engineBoards.boardsMap.at pid.traverse.boardRects.at rid .= Nothing

    mSetCursor t = do
        gui <- ask

        liftIO $ do
            c     <- traverse Gtk.cursorNew t
            frame <- Gtk.widgetGetDrawWindow $ guiDrawingArea gui
            Gtk.drawWindowSetCursor frame c

    mFreshId =
        engineDrawState.drawFreshId <+= 1

--------------------------------------------------------------------------------
runNormal :: GUI -> NormalMode a -> EngineState -> IO EngineState
runNormal gui (NormalMode m)  s = do
    (s', _) <- execRWST m gui s
    return s'

--------------------------------------------------------------------------------
normalMode :: GUI -> Mode
normalMode gui = Mode (runNormal gui . runM)
