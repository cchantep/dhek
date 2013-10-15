module Main where

import Graphics.UI.Gtk
import Dhek.Views (createPdfChooserDialog, windowParams, createMenuBar)
import Dhek.ViewerRef

main :: IO ()
main = do
  initGUI
  window  <- windowNew
  vbox    <- vBoxNew False 10
  fdialog <- createPdfChooserDialog window
  createMenuBar window vbox fdialog
  containerAdd window vbox
  set window windowParams
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
