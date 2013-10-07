module Dhek.Views where

import Control.Lens
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Data.Foldable (traverse_)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef, writeIORef)
import Data.Maybe (fromJust)
import Dhek.Action
import Dhek.Callbacks
import Dhek.Types
import Dhek.Utils
import Graphics.UI.Gtk

data SelectionHandlers = SelectionHandlers
    { hOnSelection :: Rect -> IO ()
    , hOnClear     :: IO () }

createPdfChooserDialog :: Window -> IO FileChooserDialog
createPdfChooserDialog win = do
  ch <- fileChooserDialogNew title (Just win) FileChooserActionOpen responses
  filt <- fileFilterNew
  fileFilterAddPattern filt "*.pdf"
  fileFilterSetName filt "PDF File"
  fileChooserAddFilter ch filt
  return ch
    where
      responses = [("Open", ResponseOk)
                  ,("Cancel", ResponseCancel)]
      title = Just "Open a PDF file"

createJsonChooserDialog :: Window -> IO FileChooserDialog
createJsonChooserDialog win = do
  ch <- fileChooserDialogNew title (Just win) FileChooserActionSave responses
  filt <- fileFilterNew
  fileFilterAddPattern filt "*.json"
  fileFilterSetName filt "Json File"
  fileChooserAddFilter ch filt
  fileChooserSetDoOverwriteConfirmation ch True
  return ch
    where
      responses = [("Save", ResponseOk)
                  ,("Cancel", ResponseCancel)]
      title = Just "Open a Json file"

createJsonImportDialog :: Window -> IO FileChooserDialog
createJsonImportDialog win = do
  ch <- fileChooserDialogNew title (Just win) FileChooserActionOpen responses
  filt <- fileFilterNew
  fileFilterAddPattern filt "*.json"
  fileFilterSetName filt "Json File"
  fileChooserAddFilter ch filt
  return ch
    where
      responses = [("Choose", ResponseOk)
                  ,("Cancel", ResponseCancel)]
      title = Just "Choose a Json file"

windowParams :: [AttrOp Window]
windowParams =
    [windowTitle          := "Dhek PDF Viewer"
    ,windowDefaultWidth   := 800
    ,windowDefaultHeight  := 600
    ,containerBorderWidth := 10]

createMenuBar :: Window
              -> VBox
              -> FileChooserDialog
              -> IO ()
createMenuBar win vbox fdialog = do
  mbar   <- menuBarNew
  malign <- alignmentNew 0 0 1 0
  fitem  <- menuItemNewWithLabel "File"
  oitem  <- menuItemNewWithLabel "Open PDF"
  iitem  <- menuItemNewWithLabel "Load mappings"
  sitem  <- menuItemNewWithLabel "Save mappings"
  fmenu  <- menuNew
  menuShellAppend fmenu oitem
  menuShellAppend fmenu iitem
  menuShellAppend fmenu sitem
  menuItemSetSubmenu fitem fmenu
  menuShellAppend mbar fitem
  containerAdd malign mbar
  widgetSetSensitive iitem False
  widgetSetSensitive sitem False
  boxPackStart vbox malign PackNatural 0
  void $ oitem `on` menuItemActivate $
        openPdfFileChooser openPdf vbox fdialog win oitem iitem sitem

createNavButtons :: String
                 -> Window
                 -> ListStore Rect
                 -> IO ()
                 -> IORef Viewer
                 -> IO (Button, Button)
createNavButtons name win store redraw ref = do
  prev <- buttonNewWithLabel "Previous"
  next <- buttonNewWithLabel "Next"
  prev `on` buttonActivated $ onPrevious name win prev next store redraw ref
  next `on` buttonActivated $ onNext name win next prev store redraw ref
  return (prev, next)

createZoomButtons :: IO () -> IORef Viewer -> IO (Button, Button)
createZoomButtons redraw ref = do
  minus <- buttonNewWithLabel "-"
  plus  <- buttonNewWithLabel "+"
  minus `on` buttonActivated $ onCommonScale pred minus plus redraw ref
  plus  `on` buttonActivated $ onCommonScale succ minus plus redraw ref
  return (minus, plus)

createRemoveAreaButton :: TreeSelection
                       -> ListStore Rect
                       -> IO ()
                       -> IORef Viewer
                       -> IO Button
createRemoveAreaButton sel store redraw ref = do
  rem <- buttonNewWithLabel "Remove"
  rem `on` buttonActivated $ onRemoveArea sel store redraw ref
  return rem

createTreeView :: ListStore Rect -> IO () -> IORef Viewer -> IO TreeView
createTreeView store redraw ref = do
  treeV <- treeViewNewWithModel store
  col <- treeViewColumnNew
  treeViewColumnSetTitle col "Areas"
  trenderer <- cellRendererTextNew
  cellLayoutPackStart col trenderer False
  let mapping r = [cellText := r ^. rectName]
  cellLayoutSetAttributes col trenderer store mapping
  treeViewAppendColumn treeV col
  sel <- treeViewGetSelection treeV
  sel `on` treeSelectionSelectionChanged $ onTreeSelection sel store redraw ref
  return treeV

openPdf :: FileChooserDialog -> MenuItem -> MenuItem -> Window -> IO HBox
openPdf chooser mimport msave win = do
  uri    <- fmap fromJust (fileChooserGetURI chooser)
  name   <- fmap (takeFileName . fromJust) (fileChooserGetFilename chooser)
  store  <- listStoreNew ([] :: [Rect])
  area   <- drawingAreaNew
  swin   <- scrolledWindowNew Nothing Nothing
  let redraw = widgetQueueDraw area
  ref    <- makeViewer uri store
  treeV  <- createTreeView store redraw ref
  v      <- readIORef ref
  let nb  = v ^. viewerPageCount
  vbox    <- vBoxNew False 10
  hbox    <- hBoxNew False 10
  vleft   <- vBoxNew False 10
  align   <- alignmentNew 0 0 0 0
  aswin   <- alignmentNew 0 0 1 1
  arem    <- alignmentNew 0.5 0 0 0
  bbox    <- hButtonBoxNew
  scale   <- hScaleNewWithRange 100 200 1
  (prev, next)  <- createNavButtons name win store redraw ref
  (minus, plus) <- createZoomButtons redraw ref
  ifch    <- createJsonImportDialog win
  jfch    <- createJsonChooserDialog win
  sep     <- vSeparatorNew
  sel     <- treeViewGetSelection treeV
  let selection = treeSelection sel store
  rem <- createRemoveAreaButton sel store redraw ref
  scrolledWindowAddWithViewport swin area
  scrolledWindowSetPolicy swin PolicyAutomatic PolicyAutomatic
  widgetAddEvents area [PointerMotionMask]
  widgetAddEvents area [PointerMotionMask]
  area `on` exposeEvent $ tryEvent $ drawViewer area ref
  area `on` motionNotifyEvent $ tryEvent $ onMove redraw ref
  area `on` buttonPressEvent $ tryEvent $ onPress ref
  area `on` enterNotifyEvent $ tryEvent $ onEnter
  area `on` buttonReleaseEvent $ tryEvent $ onRelease
           (onAreaCreation store sel) redraw ref
  mimport `on` menuItemActivate $ void $ dialogRun ifch
  msave `on` menuItemActivate $ void $ dialogRun jfch
  ifch  `on` response $ onJsonImport ref redraw store ifch
  jfch  `on` response $ onJsonSave ref jfch
  windowSetTitle win (name ++ " (page 1 / " ++ show nb ++ ")")
  widgetSetSensitive prev False
  widgetSetSensitive next (nb /= 1)
  widgetSetSensitive mimport True
  widgetSetSensitive msave True
  containerAdd arem rem
  containerAdd align bbox
  containerAdd bbox prev
  containerAdd bbox next
  containerAdd bbox sep
  containerAdd bbox minus
  containerAdd bbox plus
  boxPackStart vbox align PackNatural 0
  containerAdd aswin swin
  boxPackStart vleft treeV PackGrow 0
  boxPackStart vleft arem PackNatural 0
  boxPackStart vbox aswin PackGrow 0
  boxPackStart hbox vbox PackGrow 0
  boxPackStart hbox vleft PackNatural 0
  handlers <- createPropView win vleft store ref
  let onSel = hOnSelection handlers
      onRem = hOnClear handlers
  sel `on` treeSelectionSelectionChanged $
          (traverse_ (onSel . snd) =<< selection)
  rem `on` buttonActivated $ onRem
  return hbox
  where
    onEnter = do
        frame  <- eventWindow
        cursor <- liftIO $ cursorNew Tcross
        liftIO $ drawWindowSetCursor frame (Just cursor)

treeSelection :: TreeSelection -> ListStore Rect -> IO (Maybe (TreeIter, Rect))
treeSelection sel store =
    traverse go =<< treeSelectionGetSelected sel
  where
    go iter =
        let idx = listStoreIterToIndex iter in
        fmap (\r -> (iter, r)) (listStoreGetValue store idx)

createPropView :: BoxClass b
               => Window
               -> b
               -> ListStore Rect
               -> IORef Viewer
               -> IO SelectionHandlers
createPropView win b rectStore ref = do
  nlabel <- labelNew (Just "Name")
  tlabel <- labelNew (Just "Type")
  updbut <- buttonNewWithLabel "Update"
  nentry <- entryNew
  salign <- alignmentNew 0 0 1 0
  ualign <- alignmentNew 0.5 0 0 0
  nalign <- alignmentNew 0 0.5 0 0
  talign <- alignmentNew 0 0.5 0 0
  tcombo <- comboBoxNew
  store  <- comboBoxSetModelText tcombo
  table  <- tableNew 2 2 False
  tvbox  <- vBoxNew False 10
  sep    <- hSeparatorNew
  containerAdd nalign nlabel
  containerAdd talign tlabel
  tableAttachDefaults table nalign 0 1 0 1
  tableAttachDefaults table nentry 1 2 0 1
  tableAttachDefaults table talign 0 1 1 2
  tableAttachDefaults table tcombo 1 2 1 2
  tableSetRowSpacings table 10
  tableSetColSpacings table 10
  traverse_ (listStoreAppend store) model
  containerAdd salign sep
  containerAdd ualign updbut
  boxPackStart tvbox table PackNatural 0
  boxPackStart tvbox ualign PackNatural 0
  boxPackStart b salign PackNatural 0
  containerAdd b tvbox
  updbut `on` buttonActivated $ onPropUpdate win rectStore nentry tcombo ref
  let hdls = SelectionHandlers
             (onPropAreaSelection nentry store tcombo)
             (onPropClear nentry tcombo)
  return hdls
    where
      model = ["text", "checkbox"]

makeViewer :: String -> ListStore Rect -> IO (IORef Viewer)
makeViewer filepath store = do
  viewer <- loadPdf filepath
  ref    <- newIORef viewer
  --registerViewerEvents store ref
  return ref
