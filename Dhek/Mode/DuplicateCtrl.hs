{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Mode.DuplicateCtrl
--
--------------------------------------------------------------------------------
module Dhek.Mode.DuplicateCtrl (duplicateCtrlModeManager) where

--------------------------------------------------------------------------------
import Control.Applicative

--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Reader
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
import Dhek.Engine.Type
import Dhek.GUI
import Dhek.Mode.Duplicate
import Dhek.Types

--------------------------------------------------------------------------------
newtype DuplicateCtrlMode a
    = DuplicateCtrlMode (DuplicateMode a)
    deriving ( Functor
             , Applicative
             , Monad
             )

--------------------------------------------------------------------------------
instance ModeMonad DuplicateCtrlMode where
    mMove opts
        = DuplicateCtrlMode $
              do mMove opts
                 gui <- ask
                 liftIO $ updatePopupPos gui

    mPress opts
        = DuplicateCtrlMode $ dupStart opts

    mRelease _
        = DuplicateCtrlMode $
              do eOpt <- use $ engineDrawState.drawEvent
                 case eOpt of
                     Just (Hold x _) -> dupEnd x
                     _               -> return ()

    mDrawing page ratio
        = DuplicateCtrlMode $ mDrawing page ratio

    mKeyPress _ = return ()

    mKeyRelease _ = return ()

--------------------------------------------------------------------------------
runDuplicateCtrl :: GUI -> DuplicateCtrlMode a -> EngineState -> IO EngineState
runDuplicateCtrl gui (DuplicateCtrlMode m) s
    = runDuplicate gui m s

--------------------------------------------------------------------------------
duplicateCtrlMode :: GUI -> Mode
duplicateCtrlMode gui = Mode (runDuplicateCtrl gui . runM)

--------------------------------------------------------------------------------
duplicateCtrlModeManager :: GUI -> IO ModeManager
duplicateCtrlModeManager gui
    = return $ ModeManager (duplicateCtrlMode gui) (return ())

--------------------------------------------------------------------------------
statusModPressed :: [Gtk.Modifier] -> Bool
statusModPressed xs
    | [Gtk.Control] <- xs = True
    | otherwise           = False

--------------------------------------------------------------------------------
statusNamePressed :: String -> Bool
statusNamePressed n
    | "Control_L" <- n = True
    | "Control_R" <- n = True
    | otherwise        = False

--------------------------------------------------------------------------------
updatePopupPos :: GUI -> IO ()
updatePopupPos g
    = do (x,y) <- Gtk.widgetGetPointer $ guiWindow g
         Gtk.windowMove (guiDrawPopup g) x (y+40)
