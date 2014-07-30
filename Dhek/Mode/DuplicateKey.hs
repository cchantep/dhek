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
import           Control.Lens
import           Control.Monad.Reader
import           Data.Foldable (for_)
import           Data.IORef
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
import Dhek.Engine.Type
import Dhek.GUI
import Dhek.Mode.Duplicate
import Dhek.Types

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
                 gui <- ask
                 liftIO $ updatePopupPos gui

    mPress opts
        = DuplicateKeyMode $ dupStart opts

    mRelease _
        = DuplicateKeyMode $
              do eOpt <- use $ engineDrawState.drawEvent
                 case eOpt of
                     Just (Hold x _) -> dupEnd x
                     _               -> return ()

    mDrawing page ratio
        = DuplicateKeyMode $ mDrawing page ratio

    mKeyPress _ = return ()

    mKeyRelease _ = return ()

--------------------------------------------------------------------------------
runDuplicateKey :: GUI -> DuplicateKeyMode a -> EngineState -> IO EngineState
runDuplicateKey gui (DuplicateKeyMode m) s
    = runDuplicate gui m s

--------------------------------------------------------------------------------
duplicateKeyMode :: GUI -> Mode
duplicateKeyMode gui = Mode (runDuplicateKey gui . runM)

--------------------------------------------------------------------------------
duplicateKeyModeManager :: GUI -> IO ModeManager
duplicateKeyModeManager gui
    = return $ ModeManager (duplicateKeyMode gui) (return ())

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
    = readIORef (guiCursorPixbuf g) >>= \opix ->
          for_ opix $ \pix ->
              do (wx,wy) <- Gtk.windowGetPosition $ guiWindow g
                 (x,y)   <- Gtk.widgetGetPointer $ guiWindow g
                 height  <- Gtk.pixbufGetHeight pix
                 -- Workaround expected to make tooltip position work on OSX
                 -- and Windows
                 Gtk.windowMove (guiDrawPopup g) (x+wx) (y-height+wy)
