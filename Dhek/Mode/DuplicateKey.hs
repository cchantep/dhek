{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Mode.DuplicateKey
--
--------------------------------------------------------------------------------
module Dhek.Mode.DuplicateKey
    ( duplicateKeyModeManager
    , updatePopupPos
    ) where

--------------------------------------------------------------------------------
import Control.Applicative

--------------------------------------------------------------------------------
import           Control.Monad.Reader
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
import Dhek.Engine.Type
import Dhek.GUI
import Dhek.Mode.Duplicate

--------------------------------------------------------------------------------
newtype DuplicateKeyMode a
    = DuplicateKeyMode (DuplicateMode a)
    deriving ( Functor
             , Applicative
             , Monad
             )

--------------------------------------------------------------------------------
instance ModeMonad DuplicateKeyMode where
    mMove opts
        = DuplicateKeyMode $
              do mMove opts
                 gui <- duplicateGetGUI
                 liftIO $
                     do Gtk.widgetShowAll $ guiDrawPopup gui
                        updatePopupPos gui

    mPress opts
        = DuplicateKeyMode $ dupStart opts

    mRelease _
        = DuplicateKeyMode $
              do mR <- duplicateGetDup
                 maybe (return ()) (\(r, v) -> dupEnd r v) mR

    mDrawing page ratio
        = DuplicateKeyMode $ mDrawing page ratio

    mKeyPress _ = return ()

    mKeyRelease _ = return ()

    mEnter = return ()

    mLeave = return ()

--------------------------------------------------------------------------------
runDuplicateKey :: ConcreteDuplicateMode
                -> DuplicateKeyMode a
                -> EngineState
                -> IO EngineState
runDuplicateKey cdm (DuplicateKeyMode m) s
    = cdmRun cdm m s

--------------------------------------------------------------------------------
duplicateKeyMode :: ConcreteDuplicateMode -> Mode
duplicateKeyMode cdm = Mode (runDuplicateKey cdm . runM)

--------------------------------------------------------------------------------
duplicateKeyModeManager :: GUI -> IO ModeManager
duplicateKeyModeManager gui
    = do cdm <- concreteDuplicateManager gui
         return $ ModeManager (duplicateKeyMode cdm) (cdmCleanup cdm)

--------------------------------------------------------------------------------
updatePopupPos :: GUI -> IO ()
updatePopupPos g
    = do (wx,wy)    <- Gtk.windowGetPosition $ guiWindow g
         (x,y)      <- Gtk.widgetGetPointer $ guiWindow g
         (_,height) <- Gtk.windowGetSize $ guiDrawPopup g
         -- Workaround expected to make tooltip position work on OSX
         -- and Windows
         Gtk.windowMove (guiDrawPopup g) (x+wx) (y-height+wy-20)
