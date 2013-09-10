module Main where

import Action
  (
   onPrevState, onNextState, onNavButton,
   askDrawingViewer, loadPdf, registerViewerEvents
  )
import Control.Monad (when, void)
import Control.Monad.Trans (liftIO)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef, writeIORef)
import Data.Foldable (traverse_)
import qualified Data.ByteString.Lazy as B
import Data.Aeson (encode, decode)
import Data.Maybe (fromJust)
import qualified Data.IntMap as I
import Graphics.UI.Gtk
import Types (Viewer(..), Save(..), RectStore(..), fillUp)
import Utils

main :: IO ()
main = do
  initGUI
  window <- windowNew
  malign <- alignmentNew 0 0 1 0
  mbar   <- menuBarNew
  mitem  <- menuItemNewWithLabel "File"
  fmenu  <- menuNew
  mopen  <- menuItemNewWithLabel "Open"
  msave  <- menuItemNewWithLabel "Save"
  widgetSetSensitive msave False
  vbox   <- vBoxNew False 10
  fdialog <- createPdfChooserDialog window
  mopen `on` menuItemActivate $ openFileChooser vbox fdialog window mopen msave
  menuShellAppend fmenu mopen
  menuShellAppend fmenu msave
  menuItemSetSubmenu mitem fmenu
  containerAdd malign mbar
  menuShellAppend mbar mitem
  containerAdd window vbox
  boxPackStart vbox malign PackNatural 0
  set window windowParams
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

createPdfChooserDialog :: Window -> IO FileChooserDialog
createPdfChooserDialog win = do
  ch <- fileChooserDialogNew (Just "Open a PDF file") (Just win) FileChooserActionOpen [("Open", ResponseOk), ("Cancel", ResponseCancel)]
  filt <- fileFilterNew
  fileFilterAddPattern filt "*.pdf"
  fileFilterSetName filt "PDF File"
  fileChooserAddFilter ch filt
  return ch

createJsonChooserDialog :: Window -> IO FileChooserDialog
createJsonChooserDialog win = do
  ch <- fileChooserDialogNew (Just "Open a Json file") (Just win) FileChooserActionSave [("Save", ResponseOk), ("Cancel", ResponseCancel)]
  filt <- fileFilterNew
  fileFilterAddPattern filt "*.json"
  fileFilterSetName filt "Json File"
  fileChooserAddFilter ch filt
  return ch

openFileChooser :: VBox
                -> FileChooserDialog
                -> Window
                -> MenuItem
                -> MenuItem
                -> IO ()
openFileChooser vbox dialog win mopen msave = do
  resp <- dialogRun dialog
  widgetHide dialog
  case resp of
    ResponseCancel -> return ()
    ResponseOk     -> do
      avbox <- alignmentNew 0 0 1 1
      vvbox <- openPdf dialog msave win
      containerAdd avbox vvbox
      boxPackStart vbox avbox PackGrow 0
      widgetSetSensitive mopen False
      widgetShowAll avbox

createPageInfoPanel :: IO TreeView
createPageInfoPanel = treeViewNew

createControlPanel :: VBox -> IO Alignment
createControlPanel vbox = do
  align  <- alignmentNew 1 0 0 0
  bbox   <- hButtonBoxNew
  label  <- labelNew Nothing
  spinB  <- spinButtonNewWithRange 0 0 1
  (prev, nxt) <- createNavButtons
  widgetSetSensitive spinB False
  widgetSetSensitive prev False
  widgetSetSensitive nxt False
  containerAdd align bbox
  containerAdd bbox prev
  containerAdd bbox spinB
  containerAdd bbox label
  containerAdd bbox nxt
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

openPdf :: FileChooserDialog -> MenuItem -> Window -> IO VBox
openPdf chooser msave win = do
  uri <- fmap fromJust (fileChooserGetURI chooser)
  name <- fmap (takeFileName . fromJust) (fileChooserGetFilename chooser)
  ref <- makeViewer uri
  v   <- readIORef ref
  let nb   = viewerPageCount v
      swin = viewerScrolledWindow v
  vbox    <- vBoxNew False 10
  align   <- alignmentNew 0 0 0 0
  aswin   <- alignmentNew 0 0 1 1
  bbox    <- hButtonBoxNew
  scale   <- hScaleNewWithRange 100 200 1
  prev    <- buttonNewWithLabel "Previous"
  next    <- buttonNewWithLabel "Next"
  minus   <- buttonNewWithLabel "-"
  plus    <- buttonNewWithLabel "+"
  jfch    <- createJsonChooserDialog win
  file    <- fileChooserSetDoOverwriteConfirmation jfch True
  sep     <- vSeparatorNew
  prev  `on` buttonActivated $ onPrev name prev next ref
  next  `on` buttonActivated $ onNext name next prev ref
  minus `on` buttonActivated $ onCommonScale pred minus plus ref
  plus  `on` buttonActivated $ onCommonScale succ minus plus ref
  msave `on` menuItemActivate $ void $ dialogRun jfch
  jfch  `on` response $ onJsonSave ref jfch
  windowSetTitle win (name ++ " (page 1 / " ++ show nb ++ ")")
  widgetSetSensitive prev False
  widgetSetSensitive next (nb /= 1)
  widgetSetSensitive msave True
  containerAdd align bbox
  containerAdd bbox prev
  containerAdd bbox next
  containerAdd bbox sep
  containerAdd bbox minus
  containerAdd bbox plus
  boxPackStart vbox align PackNatural 0
  containerAdd aswin swin
  boxPackStart vbox aswin PackGrow 0
  return vbox
    where
      onPrev name prev next ref = onCommon name onPrevState prev next ref
      onNext name next prev ref = onCommon name onNextState next prev ref

      onCommon name k self other ref = do
        v <- readIORef ref
        let nb                    = viewerPageCount v
            (tSelf, tOther, newV) = onNavButton k v
            newCur                = viewerCurrentPage newV
            title = name ++ " (page " ++ show newCur ++" / " ++ show nb ++ ")"
        widgetSetSensitive self (not tSelf)
        when tOther (widgetSetSensitive other True)
        writeIORef ref newV
        windowSetTitle win title
        askDrawingViewer newV

      onJsonSave _ jfch ResponseCancel = widgetHide jfch
      onJsonSave ref jfch ResponseOk   = do
        v <- readIORef ref
        let nb    = viewerPageCount v
            rects = I.toList $ rstoreRects $ viewerStore v
            save  = Save $ fillUp nb rects
        opt <- fileChooserGetFilename jfch
        let ensure path
              | takeExtension path == ".json" = path
              | otherwise                     = path ++ ".json"
        traverse_ (\p -> B.writeFile (ensure p) (encode save)) opt
        widgetHide jfch

      onCommonScale k minus plus ref =
        let f v =
              let z   = viewerZoom v
                  z2  = k z
                  low = (z2 - 1) < 0
                  up  = (z2 + 1) > 10
                  v2  = v { viewerZoom = z2 } in
              do widgetSetSensitive minus (not low)
                 widgetSetSensitive plus (not up)
                 writeIORef ref v2
                 askDrawingViewer v2 in
        readIORef ref >>= f


    -- onSave ref = do
    --   v <- readIORef ref
    --   let nb    = viewerPageCount v
    --       rects = I.toList $ rstoreRects $ viewerStore v
    --       save  = Save $ fillUp nb rects
    --   opt <- fileChooserGetFilename jsonChooser
    --   traverse_ (\p -> B.writeFile p (encode save)) opt

createTable :: IO Table
createTable = tableNew 2 2 False

makeViewer :: String -> IO (IORef Viewer)
makeViewer filepath = do
  viewer <- loadPdf filepath
  ref    <- newIORef viewer
  registerViewerEvents ref
  return ref
