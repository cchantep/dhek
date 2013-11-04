module Main where

import Control.Lens ((.=))
import Control.Monad.State

-- import Graphics.UI.Gtk
-- import Dhek.Views (createPdfChooserDialog, windowParams, createMenuBar)
-- import Dhek.ViewerRef
import Dhek.Engine
import Dhek.Draw
import Dhek.Move

main :: IO ()
main = do
    eng <- fmap (execState conf) gtkEngineNew
    engineStart eng
  where
    conf = do
        engineDrawing .= gtkDraw
        engineMove    .= onMove

-- main = do
--   initGUI
--   window  <- windowNew
--   vbox    <- vBoxNew False 10
--   fdialog <- createPdfChooserDialog window
--   createMenuBar window vbox fdialog
--   containerAdd window vbox
--   set window windowParams
--   onDestroy window mainQuit
--   widgetShowAll window
--   mainGUI
