module Main where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Poppler.Document
import Graphics.UI.Gtk.Poppler.Page

data Viewer =
            Viewer { viewerArea           :: DrawingArea
                   , viewerDocument       :: Document
                   , viewerScrolledWindow :: ScrolledWindow }

main :: IO ()
main = do
  initGUI
  window <- windowNew
  table  <- createTable
  fchb   <- createFileChooserButton
  button <- createViewButton table fchb
  tableAttachDefaults table fchb 0 1 0 1
  tableAttachDefaults table button 1 2 0 1
  containerAdd window table
  set window windowParams
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

windowParams :: [AttrOp Window]
windowParams =
  [windowTitle          := "Dhek PDF Viewer"
  ,windowDefaultWidth   := 640
  ,windowDefaultHeight  := 480
  ,containerBorderWidth := 10]

createFileChooserButton :: IO FileChooserButton
createFileChooserButton = do
  fcb  <- fileChooserButtonNew "Select PDF File" FileChooserActionOpen
  filt <- fileFilterNew
  fileFilterAddPattern filt "*.pdf"
  fileFilterSetName filt "Pdf File"
  fileChooserAddFilter fcb  filt
  return fcb

createViewButton :: Table -> FileChooserButton -> IO Button
createViewButton table chooser = do
  button <- buttonNewWithLabel "View"
  onClicked button go
  return button

  where
    go = do
      select <- fileChooserGetFilename chooser
      maybe (print "(No Selection)") makeView select

    makeView filepath = do
      (Viewer area doc swin) <- viewerNew ("file://" ++ filepath)
      vbox <- vBoxNew False 0
      tableAttachDefaults table vbox 0 2 1 2
      boxPackStart vbox swin PackGrow 0


createTable :: IO Table
createTable = tableNew 2 2 True

viewerNew :: String -> IO Viewer
viewerNew filepath = do
  area <- drawingAreaNew
  doc  <- liftM (\(Just x) -> x) (documentNewFromFile filepath Nothing)
  swin <- scrolledWindowNew Nothing Nothing
  scrolledWindowAddWithViewport swin area
  scrolledWindowSetPolicy swin PolicyAutomatic PolicyAutomatic
  let viewer = Viewer area  doc swin
  area `on` exposeEvent $ tryEvent $ viewerDraw viewer
  return viewer

viewerDraw :: Viewer -> EventM EExpose ()
viewerDraw = liftIO . go
  where
    go (Viewer area doc swin) = do
      page  <- documentGetPage doc 1
      frame <- widgetGetDrawWindow area
      (docWidth, docHeight) <- pageGetSize page
      let scaleX = 200 / docWidth
          width  = 200
          height = scaleX * docHeight
      widgetSetSizeRequest area (truncate width) (truncate height)
      renderWithDrawable frame (setSourceRGB 1.0 1.0 1.0 >>
                                scale scaleX scaleX      >>
                                pageRender page)
