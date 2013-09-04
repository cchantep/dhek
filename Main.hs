module Main where

import Action
  (
   onPrevState, onNextState, onNavButton,
   askDrawingViewer, loadPdf, registerViewerEvents
  )
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef, writeIORef)
import Data.Foldable (traverse_)
import qualified Data.ByteString.Lazy as B
import Data.Aeson (encode, decode)
import qualified Data.IntMap as I
import Graphics.UI.Gtk
import Types (Viewer(..), Save(..), fillUp)

main :: IO ()
main = do
  initGUI
  window   <- windowNew
  hbox     <- hBoxNew False 10
  pageNav  <- vBoxNew False 0
  pageInfo <- vBoxNew False 0
  treeV    <- createPageInfoPanel
  align    <- createControlPanel pageNav
  boxPackStart pageNav align PackNatural 10
  containerAdd pageInfo treeV
  containerAdd hbox pageNav
  containerAdd hbox pageInfo
  containerAdd window hbox
  set window windowParams
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

createPageInfoPanel :: IO TreeView
createPageInfoPanel = treeViewNew

createControlPanel :: VBox -> IO Alignment
createControlPanel vbox = do
  align  <- alignmentNew 1 0 0 0
  bbox   <- hButtonBoxNew
  fchb   <- createFileChooserButton
  fchj   <- createJsonFileChooserButton
  label  <- labelNew Nothing
  spinB  <- spinButtonNewWithRange 0 0 1
  scale  <- hScaleNewWithRange 1 200 1
  (prev, nxt) <- createNavButtons
  save   <- buttonNewWithLabel "Save Json"
  button <- createViewButton vbox fchb fchj nxt prev save label spinB scale
  widgetSetSensitive spinB False
  widgetSetSensitive scale False
  widgetSetSensitive prev False
  widgetSetSensitive nxt False
  rangeSetValue scale 100
  containerAdd align bbox
  containerAdd bbox prev
  containerAdd bbox spinB
  containerAdd bbox label
  containerAdd bbox nxt
  containerAdd bbox fchb
  containerAdd bbox fchj
  containerAdd bbox scale
  containerAdd bbox button
  containerAdd bbox save
  set bbox [buttonBoxLayoutStyle := ButtonboxStart]
  return align

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

createJsonFileChooserButton :: IO FileChooserButton
createJsonFileChooserButton = do
  fcb  <- fileChooserButtonNew "Select Json File" FileChooserActionOpen
  filt <- fileFilterNew
  fileFilterAddPattern filt "*.json"
  fileFilterSetName filt "Json File"
  fileChooserAddFilter fcb filt
  return fcb

createNavButtons :: IO (Button, Button)
createNavButtons = do
  predB <- buttonNewWithLabel "Previous"
  nextB <- buttonNewWithLabel "Next"
  return (predB, nextB)

createViewButton :: VBox
                 -> FileChooserButton
                 -> FileChooserButton
                 -> Button
                 -> Button
                 -> Button
                 -> Label
                 -> SpinButton
                 -> HScale
                 -> IO Button
createViewButton vbox chooser jsonChooser nxt prev save label spinB scale = do
  button <- buttonNewWithLabel "View"
  button `on` buttonActivated $ go button
  return button

  where
    go button = do
      select <- fileChooserGetURI chooser
      maybe (print "(No Selection)") (makeView button) select

    makeView button filepath = do
      ref <- makeViewer filepath
      v   <- readIORef ref
      let pagesStr    = show $ viewerPageCount v
          charLength  = length pagesStr
          doc         = viewerDocument v
          swin        = viewerScrolledWindow v
          cur         = viewerCurrentPage v
          nPages      = viewerPageCount v
          onlyOnePage = nPages == 1
      labelSetText label ("/ " ++ pagesStr)
      spinButtonSetValue spinB (fromIntegral cur)
      spinButtonSetRange spinB 1 (fromIntegral nPages)
      boxPackStart vbox swin PackGrow 0
      widgetSetSensitive spinB True
      widgetSetSensitive chooser False
      widgetSetSensitive button False
      widgetSetSensitive scale True
      widgetSetSensitive prev False
      widgetSetSensitive nxt (not onlyOnePage)
      prev  `on` buttonActivated $ onPrev ref
      nxt   `on` buttonActivated $ onNext ref
      save  `on` buttonActivated $ onSave ref
      scale `on` valueChanged $ pageZoomChanged ref
      onValueSpinned spinB (pageBrowserChanged ref)
      widgetShowAll vbox

    onPrev ref = onCommon onPrevState prev nxt ref
    onNext ref = onCommon onNextState nxt prev ref

    onCommon k self other ref = do
      v <- readIORef ref
      let (tSelf, tOther, newV) = onNavButton k v
          newCur                = viewerCurrentPage newV
      widgetSetSensitive self (not tSelf)
      when tOther (widgetSetSensitive other True)
      spinButtonSetValue spinB (fromIntegral newCur)
      writeIORef ref newV
      askDrawingViewer newV

    pageBrowserChanged ref = do
      value <- spinButtonGetValueAsInt spinB
      v     <- readIORef ref
      let newV = v { viewerCurrentPage = value }
          nb   = viewerPageCount v
      writeIORef ref newV
      when (value - 1 < 1) (widgetSetSensitive prev False)
      when (value + 1 > nb) (widgetSetSensitive nxt False)
      when (value - 1 >= 1) (widgetSetSensitive prev True)
      when (value + 1 <= nb) (widgetSetSensitive nxt True)
      askDrawingViewer newV

    pageZoomChanged ref = do
      value <- rangeGetValue scale
      v     <- readIORef ref
      let newV = v { viewerZoom = value / 100 }
      writeIORef ref newV
      askDrawingViewer newV

    onSave ref = do
      v <- readIORef ref
      let rects = viewerRects v
          nb    = viewerPageCount v
          save  = Save $ fillUp nb (I.toList rects)
      opt <- fileChooserGetFilename jsonChooser
      traverse_ (\p -> B.writeFile p (encode save)) opt


createTable :: IO Table
createTable = tableNew 2 2 False

makeViewer :: String -> IO (IORef Viewer)
makeViewer filepath = do
  viewer <- loadPdf filepath
  ref    <- newIORef viewer
  registerViewerEvents ref
  return ref
