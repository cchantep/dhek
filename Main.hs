module Main where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Char (isDigit)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Poppler.Document
import Graphics.UI.Gtk.Poppler.Page

data Viewer =
            Viewer { viewerArea           :: DrawingArea
                   , viewerDocument       :: Document
                   , viewerScrolledWindow :: ScrolledWindow
                   , viewerCurrentPage    :: Int
                   , viewerPageCount      :: Int }

main :: IO ()
main = do
  initGUI
  window <- windowNew
  vbox   <- vBoxNew False 0
  align  <- createControlPanel vbox
  boxPackStart vbox align PackNatural 10
  containerAdd window vbox
  set window windowParams
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

createViewerVar :: IO (TVar (Maybe Viewer))
createViewerVar = newTVarIO Nothing

createControlPanel :: VBox -> IO Alignment
createControlPanel vbox = do
  vVar   <- createViewerVar
  align  <- alignmentNew 1 0 0 0
  bbox   <- hButtonBoxNew
  fchb   <- createFileChooserButton
  label  <- labelNew Nothing
  entry  <- entryNew
  button <- createViewButton vbox fchb label entry vVar
  entry `on` entryActivate $ pageBrowserChanged entry vVar
  containerAdd align bbox
  containerAdd bbox entry
  containerAdd bbox label
  containerAdd bbox fchb
  containerAdd bbox button
  set bbox [buttonBoxLayoutStyle := ButtonboxStart]
  return align

pageBrowserChanged :: Entry -> TVar (Maybe Viewer) -> IO ()
pageBrowserChanged entry viewerVar = do
  text <- entryGetText entry
  when (all isDigit text) (join $ atomically $ action (read text))
    where
      action page =
        readTVar viewerVar >>= \vOpt ->
          let nothingToDo = return (return ())
              go (Viewer area y swin cur nb)
                | page == cur = nothingToDo
                | page < 1    = return (entrySetText entry "1")
                | page > nb   = return (entrySetText entry (show nb))
                | otherwise   =
                  let newViewer = Viewer area y swin (page - 1) nb in
                  writeTVar viewerVar (Just newViewer) >>= \_ ->
                    return (widgetQueueDraw area) in
          maybe nothingToDo go vOpt

windowParams :: [AttrOp Window]
windowParams =
  [windowTitle          := "Dhek PDF Viewer"
  ,windowDefaultWidth   := 800
  ,windowDefaultHeight  := 600
  ,containerBorderWidth := 10]

createFileChooserButton :: IO FileChooserButton
createFileChooserButton = do
  fcb  <- fileChooserButtonNew "Select PDF File" FileChooserActionOpen
  filt <- fileFilterNew
  fileFilterAddPattern filt "*.pdf"
  fileFilterSetName filt "Pdf File"
  fileChooserAddFilter fcb  filt
  return fcb

createViewButton :: VBox
                 -> FileChooserButton
                 -> Label
                 -> Entry
                 -> TVar (Maybe Viewer)
                 -> IO Button
createViewButton vbox chooser label entry viewerVar = do
  button <- buttonNewWithLabel "View"
  onClicked button go
  return button

  where
    go = do
      select <- fileChooserGetFilename chooser
      maybe (print "(No Selection)") makeView select

    makeView filepath = do
      updateViewer filepath viewerVar
      join $ atomically action
      widgetShowAll vbox

    action =
      readTVar viewerVar >>= \(Just (Viewer _ _ swin cur nPages)) ->
        return $ do
          let pagesStr   = show nPages
              charLength = length pagesStr
          labelSetText label ("/ " ++ pagesStr)
          entrySetText entry (show (cur +  1))
          entrySetMaxLength entry charLength
          entrySetWidthChars entry charLength
          boxPackStart vbox swin PackGrow 0

createTable :: IO Table
createTable = tableNew 2 2 False

updateViewer :: String -> TVar (Maybe Viewer) -> IO ()
updateViewer filepath var = do
  area <- drawingAreaNew
  doc  <- liftM (\(Just x) -> x) (documentNewFromFile ("file://" ++ filepath) Nothing)
  swin <- scrolledWindowNew Nothing Nothing
  scrolledWindowAddWithViewport swin area
  scrolledWindowSetPolicy swin PolicyAutomatic PolicyAutomatic
  nPages <- documentGetNPages doc
  let viewer = Viewer area  doc swin 0 nPages
  atomically $ writeTVar var (Just viewer)
  void $ area `on` exposeEvent $ tryEvent $ viewerDraw var

viewerDraw :: TVar (Maybe Viewer) -> EventM EExpose ()
viewerDraw = liftIO . (go =<<) . readTVarIO
  where
    go (Just (Viewer area doc swin cur _)) = do
      page  <- documentGetPage doc cur
      frame <- widgetGetDrawWindow area
      (docWidth, docHeight) <- pageGetSize page
      let width  = 760
          scaleX = width / docWidth
          height = scaleX * docHeight
      widgetSetSizeRequest area (truncate width) (truncate height)
      renderWithDrawable frame (setSourceRGB 1.0 1.0 1.0 >>
                                scale scaleX scaleX      >>
                                pageRender page)
