module Dhek.Views where

import Control.Lens hiding (set)
import Control.Monad (void, when)
import Control.Monad.Trans (liftIO)

import Data.Foldable (traverse_)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Maybe (fromJust)

import Dhek.Action
import Dhek.Callbacks
import Dhek.Types
import Dhek.Utils (takeFileName, trimString)
import Dhek.ViewerRef

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

createTreeView :: ListStore Rect -> TreeView -> IO ()
createTreeView store treeV = do
  col <- treeViewColumnNew
  treeViewColumnSetTitle col "Areas"
  trenderer <- cellRendererTextNew
  cellLayoutPackStart col trenderer False
  let mapping r = [cellText := r ^. rectName]
  cellLayoutSetAttributes col trenderer store mapping
  void $ treeViewAppendColumn treeV col

openPdf :: FileChooserDialog -> MenuItem -> MenuItem -> Window -> IO HBox
openPdf chooser mimport msave win = do
  uri    <- fmap fromJust (fileChooserGetURI chooser)
  name   <- fmap (takeFileName . fromJust) (fileChooserGetFilename chooser)
  store  <- listStoreNew ([] :: [Rect])
  area   <- drawingAreaNew
  hadj   <- adjustmentNew 0 0 0 0 0 0
  vadj   <- adjustmentNew 0 0 0 0 0 0
  viewport <- viewportNew hadj vadj
  containerAdd viewport area
  hscroll <- hScrollbarNew hadj
  vscroll <- vScrollbarNew vadj
  tswin  <- scrolledWindowNew Nothing Nothing
  ref    <- makeViewer uri store
  treeV  <- treeViewNewWithModel store
  sel    <- treeViewGetSelection treeV
  v      <- readIORef ref
  vruler <- vRulerNew
  hruler <- hRulerNew
  halign <- alignmentNew 0 0 1 1
  valign <- alignmentNew 0 0 0 1
  let vRef      = viewerRef ref area hscroll vscroll hadj vadj store sel hruler vruler win
      redraw    = viewerDraw vRef
      selection = viewerGetTreeSelection vRef
      nb        = v ^. viewerPageCount
  set vruler [rulerMetric := Pixels]
  set hruler [rulerMetric := Pixels]
  vbox    <- vBoxNew False 10
  hbox    <- hBoxNew False 10
  vleft   <- vBoxNew False 10
  align   <- alignmentNew 0 0 0 0
  aswin   <- alignmentNew 0 0 1 1
  atswin  <- alignmentNew 0 0 1 1
  arem    <- alignmentNew 0.5 0 0 0
  bbox    <- hButtonBoxNew
  scale   <- hScaleNewWithRange 100 200 1
  (prev, next)  <- createNavButtons name win store redraw ref
  (minus, plus) <- createZoomButtons redraw ref
  ifch    <- createJsonImportDialog win
  jfch    <- createJsonChooserDialog win
  sep     <- vSeparatorNew
  propNameEntry <- entryNew
  propTypeCombo <- comboBoxNew
  createTreeView store treeV
  rem <- createRemoveAreaButton sel store redraw ref
  scrolledWindowAddWithViewport tswin treeV
  scrolledWindowSetPolicy tswin PolicyAutomatic PolicyAutomatic
  widgetAddEvents area [PointerMotionMask]
  widgetSetSizeRequest viewport 200 200
  widgetSetSizeRequest hruler 25 25
  widgetSetSizeRequest vruler 25 25
  area `on` scrollEvent $ tryEvent $ do
      dir <- eventScrollDirection
      liftIO $ do
          pageSize <- adjustmentGetPageSize vadj
          lower    <- adjustmentGetLower vadj
          upper    <- adjustmentGetUpper vadj
          step     <- adjustmentGetStepIncrement vadj
          oldValue <- adjustmentGetValue vadj
          let delta' = step * 2
              delta  = case dir of
                  ScrollUp -> negate delta'
                  _        -> delta'
              newValue = min (upper - pageSize) (max 0 (oldValue + delta))
          adjustmentSetValue vadj newValue

  vruler `on` buttonPressEvent $ tryEvent $ do
      liftIO $ viewerGuideNew vRef GuideVertical
  vruler `on` motionNotifyEvent $ tryEvent $ do
      (x',y') <- eventCoordinates
      liftIO $ do
          ratio <- viewerGetRatio vRef
          v     <- adjustmentGetValue hadj
          let (x,y) = ((x'-25+v)/ratio, y'/ratio)
          viewerReportPosition vRef x y
          viewerGuideUpdate vRef
          viewerDraw vRef
  vruler `on` buttonReleaseEvent $ tryEvent $ liftIO $ do
      viewerGuideAdd vRef
      viewerDraw vRef

  hruler `on` buttonPressEvent $ tryEvent $ do
      liftIO $ viewerGuideNew vRef GuideHorizontal
  hruler `on` motionNotifyEvent $ tryEvent $ do
      (x',y') <- eventCoordinates
      liftIO $ do
          ratio <- viewerGetRatio vRef
          v     <- adjustmentGetValue vadj
          let (x,y) = (x'/ratio, (y'+v-25)/ratio)
          viewerReportPosition vRef x y
          viewerGuideUpdate vRef
          viewerDraw vRef
  hruler `on` buttonReleaseEvent $ tryEvent $ liftIO $ do
      viewerGuideAdd vRef
      viewerDraw vRef

  area `on` exposeEvent $ tryEvent $ drawViewer area vRef
  area `on` motionNotifyEvent $ tryEvent $ onMove vRef
  area `on` buttonPressEvent $ tryEvent $ onPress vRef
  area `on` enterNotifyEvent $ tryEvent $ onEnter
  area `on` buttonReleaseEvent $ tryEvent $ onRelease vRef
  mimport `on` menuItemActivate $ void $ dialogRun ifch
  msave `on` menuItemActivate $ void $ dialogRun jfch
  ifch  `on` response $ onJsonImport ref redraw store ifch
  jfch  `on` response $ onJsonSave ref jfch
  sel `on` treeSelectionSelectionChanged $ onTreeSelection vRef
  onEntryActivated (onPropEntryActivated vRef) propNameEntry
  onComboChanged (onPropComboChanged vRef) propTypeCombo
  windowSetTitle win (name ++ " (page 1 / " ++ show nb ++ ")")
  widgetSetSensitive prev False
  widgetSetSensitive next (nb /= 1)
  widgetSetSensitive mimport True
  widgetSetSensitive msave True
  atable <- tableNew 3 3 False
  tableSetRowSpacing atable 0 0
  tableSetColSpacing atable 0 0
  tableAttach atable hruler 1 2 0 1 [Expand, Shrink, Fill] [Fill] 0 0
  tableAttach atable hscroll 1 2 2 3 [Expand, Shrink, Fill] [Fill] 0 0
  tableAttach atable vruler 0 1 1 2 [Fill] [Expand, Shrink, Fill] 0 0
  tableAttach atable vscroll 2 3 1 2 [Fill] [Expand, Shrink, Fill] 0 0
  tableAttach atable viewport 1 2 1 2 [Expand, Fill] [Fill, Expand] 0 0
  containerAdd arem rem
  containerAdd align bbox
  containerAdd bbox prev
  containerAdd bbox next
  containerAdd bbox sep
  containerAdd bbox minus
  containerAdd bbox plus
  boxPackStart vbox align PackNatural 0
  containerAdd atswin tswin
  boxPackStart vleft atswin PackGrow 0
  boxPackStart vleft arem PackNatural 0
  boxPackStart vbox atable PackGrow 0
  boxPackStart hbox vbox PackGrow 0
  boxPackStart hbox vleft PackNatural 0
  handlers <- createPropView win vleft propNameEntry propTypeCombo store ref
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

createPropView :: BoxClass b
               => Window
               -> b
               -> Entry
               -> ComboBox
               -> ListStore Rect
               -> IORef Viewer
               -> IO SelectionHandlers
createPropView win b nentry tcombo rectStore ref = do
  nlabel <- labelNew (Just "Name")
  tlabel <- labelNew (Just "Type")
  salign <- alignmentNew 0 0 1 0
  ualign <- alignmentNew 0.5 0 0 0
  nalign <- alignmentNew 0 0.5 0 0
  talign <- alignmentNew 0 0.5 0 0
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
  boxPackStart tvbox table PackNatural 0
  boxPackStart b salign PackNatural 0
  containerAdd b tvbox
  let hdls = SelectionHandlers
             (onPropAreaSelection nentry store tcombo)
             (onPropClear nentry tcombo)
  return hdls
    where
      model = ["text", "checkbox"]

onEntryActivated :: EntryClass entry => (String -> IO ()) -> entry -> IO ()
onEntryActivated k entry =
    void $ on entry entryActivate $ do
        text' <- entryGetText entry
        let text     = trimString text'
            notEmpty = not $ null text
        when notEmpty (k text)

onComboChanged :: ComboBoxClass combo => (String -> IO ()) -> combo -> IO ()
onComboChanged k combo =
    void $ on combo changed $
         traverse_ k =<< comboBoxGetActiveText combo

makeViewer :: String -> ListStore Rect -> IO (IORef Viewer)
makeViewer filepath store = do
  viewer <- loadPdf filepath
  ref    <- newIORef viewer
  return ref
