module Main where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Char (isDigit)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Poppler.Document
import Graphics.UI.Gtk.Poppler.Annotation
import Graphics.UI.Gtk.Poppler.Page

data Viewer =
            Viewer { viewerArea           :: DrawingArea
                   , viewerDocument       :: Document
                   , viewerScrolledWindow :: ScrolledWindow
                   , viewerCurrentPage    :: Int
                   , viewerPageCount      :: Int
                   , viewerZoom           :: Double }

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
  spinB  <- spinButtonNewWithRange 0 0 1
  scale  <- hScaleNewWithRange 1 200 1
  (prev, nxt) <- createNavButtons spinB vVar
  button <- createViewButton vbox fchb nxt prev label spinB scale vVar
  widgetSetSensitive spinB False
  widgetSetSensitive scale False
  widgetSetSensitive prev False
  widgetSetSensitive nxt False
  rangeSetValue scale 100
  onValueSpinned spinB (pageBrowserChanged spinB vVar)
  scale `on` valueChanged $ pageZoomChanged scale vVar
  containerAdd align bbox
  containerAdd bbox prev
  containerAdd bbox spinB
  containerAdd bbox label
  containerAdd bbox nxt
  containerAdd bbox fchb
  containerAdd bbox scale
  containerAdd bbox button
  set bbox [buttonBoxLayoutStyle := ButtonboxStart]
  prev `on` buttonActivated $ updateEntry spinB vVar
  nxt `on` buttonActivated $ updateEntry spinB vVar
  return align

    where
      updateEntry spinB vVar = do
        (Just v) <- readTVarIO vVar
        spinButtonSetValue spinB
                          (fromIntegral $ succ $ viewerCurrentPage v)

pageBrowserChanged :: SpinButton -> TVar (Maybe Viewer) -> IO ()
pageBrowserChanged spinB viewerVar = do
  value <- spinButtonGetValueAsInt spinB
  join $ atomically $ action value
    where
      action page =
        readTVar viewerVar >>= \vOpt ->
          let nothingToDo = return (return ())
              go (Viewer area x swin cur nb y) =
                 let newViewer = Viewer area x swin (page - 1) nb y in
                 writeTVar viewerVar (Just newViewer) >>= \_ ->
                   return (widgetQueueDraw area) in
          maybe nothingToDo go vOpt

pageZoomChanged :: HScale -> TVar (Maybe Viewer) -> IO ()
pageZoomChanged scale viewerVar = do
  value <- rangeGetValue scale
  join $ atomically $ action (value / 100)
    where
      action value = do
        (Just v) <- readTVar viewerVar
        let area = viewerArea v
        writeTVar viewerVar (Just v{viewerZoom = value})
        return (widgetQueueDraw area)

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

createNavButtons :: SpinButton -> TVar (Maybe Viewer) -> IO (Button, Button)
createNavButtons spinB viewerVar = do
  predB <- buttonNewWithLabel "Previous"
  nextB <- buttonNewWithLabel "Next"
  predB `on` buttonActivated $ onPred predB nextB
  nextB `on` buttonActivated $ onNext predB nextB
  onValueSpinned spinB (updateButtonsState spinB predB nextB)
  return (predB, nextB)
    where
      onPred predB nextB =
        let inactivePrev curPage _ = curPage - 2 < 0
            action = common inactivePrev pred predB nextB
            stm = maybe nothingToDo action =<< readTVar viewerVar in
        join $ atomically stm

      onNext predB nextB =
        let inactiveNext curPage nb = curPage + 2 > (nb - 1)
            action = common inactiveNext succ nextB predB
            stm = maybe nothingToDo action =<< readTVar viewerVar in
        join $ atomically stm

      common k upd self target v =
        let curr   = viewerCurrentPage v
            nb     = viewerPageCount v
            area   = viewerArea v
            action = do
              if k curr nb
                 then widgetSetSensitive self False
                 else widgetSetSensitive target True
              widgetQueueDraw area in
        fmap (const action) $
        writeTVar viewerVar (Just (v{viewerCurrentPage=upd curr}))

      updateButtonsState spinB predB nextB =
        do (Just v) <- readTVarIO viewerVar
           value <- spinButtonGetValueAsInt spinB
           let nb = viewerPageCount v
           when (value - 1 < 1) (widgetSetSensitive predB False)
           when (value + 1 > nb) (widgetSetSensitive nextB False)
           when (value - 1 >= 1) (widgetSetSensitive predB True)
           when (value + 1 <= nb) (widgetSetSensitive nextB True)

      nothingToDo = return (return ())

createViewButton :: VBox
                 -> FileChooserButton
                 -> Button
                 -> Button
                 -> Label
                 -> SpinButton
                 -> HScale
                 -> TVar (Maybe Viewer)
                 -> IO Button
createViewButton vbox chooser nxt prev label spinB scale viewerVar = do
  button <- buttonNewWithLabel "View"
  onClicked button (go button)
  return button

  where
    go button = do
      select <- fileChooserGetFilename chooser
      maybe (print "(No Selection)") (makeView button) select

    makeView button filepath = do
      updateViewer filepath viewerVar
      join $ atomically $ action button
      widgetShowAll vbox

    action button =
      readTVar viewerVar >>= \(Just (Viewer _ doc swin cur nPages _)) ->
        return $ do
          let pagesStr   = show nPages
              charLength = length pagesStr
          labelSetText label ("/ " ++ pagesStr)
          spinButtonSetValue spinB (fromIntegral (cur + 1))
          spinButtonSetRange spinB 1 (fromIntegral nPages)
          boxPackStart vbox swin PackGrow 0
          widgetSetSensitive spinB True
          widgetSetSensitive chooser False
          widgetSetSensitive button False
          widgetSetSensitive scale True
          widgetSetSensitive prev False
          if nPages == 1
             then widgetSetSensitive nxt False
             else widgetSetSensitive nxt True

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
  let viewer = Viewer area  doc swin 0 nPages 1
  atomically $ writeTVar var (Just viewer)
  void $ area `on` exposeEvent $ tryEvent $ viewerDraw var

viewerDraw :: TVar (Maybe Viewer) -> EventM EExpose ()
viewerDraw = liftIO . (go =<<) . readTVarIO
  where
    go (Just (Viewer area doc swin cur _ zoom)) = do
      page  <- documentGetPage doc cur
      frame <- widgetGetDrawWindow area
      (docWidth, docHeight) <- pageGetSize page
      let width  = 760 * zoom
          scaleX = (width / docWidth)
          height = scaleX * docHeight
      widgetSetSizeRequest area (truncate width) (truncate height)
      renderWithDrawable frame (setSourceRGB 1.0 1.0 1.0 >>
                                scale scaleX scaleX      >>
                                pageRender page)
