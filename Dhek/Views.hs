{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Dhek.Views where

import Control.Lens
import Control.Monad (void, when, (<=<))
import Control.Monad.Trans (liftIO)
import qualified Control.Monad.State as State
import Data.Array
import Data.Foldable (foldMap, traverse_)
import Data.Functor ((<$))
import qualified Data.IntMap as I
import Data.IORef (IORef, newIORef, readIORef, modifyIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.Monoid (First(..))
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
  ref    <- makeViewer uri store
  let redraw          = widgetQueueDraw area
      updateStore     = _updateRectStore store
      appendStore     = _appendRectStore store ref
      withRectIter    = withRectStoreIter store
      viewerEvent     = _viewerGetEvent ref
      viewerSelection = _viewerGetSelection ref
      updateRect      = _viewerUpdateRect store ref
      overedRect      = _viewerGetOveredRect ref
      overedArea      = _viewerGetOveredArea ref
      setSelection    = _viewerSetSelection ref
      viewerRatio     = _viewerGetRatio ref
      viewerRects     = _viewerGetPageRects ref
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
  propNameEntry <- entryNew
  propTypeCombo <- comboBoxNew
  let selection      = treeSelection sel store
      selectItem     = selectTreeItem sel ref
      selectRect     = _selectRectItem store selectItem
      setEvent       = _viewerSetEvent ref
      modifyEvent    = _viewerModifyEvent ref
      clearSelection = _viewerClearSelection ref
      modifySelect   = _viewerModifySelection ref
      lookupRect     = _viewerLookupIter ref
      viewerSelected = _viewerGetSelected ref
      viewerOvered   = _viewerGetOvered ref
      setOvered      = _viewerSetOvered ref
      viewerPageItem = _viewerPageItem ref
      setSelected    = _viewerSetSelected ref
      vRef = ViewerRef redraw (withRectIter selectItem <=< appendStore)
             appendStore viewerEvent setEvent modifyEvent
             viewerSelection setSelection modifySelect clearSelection
             viewerSelected setSelected viewerOvered setOvered
             updateRect overedRect overedArea viewerRatio viewerRects
             selectRect lookupRect viewerPageItem win
  rem <- createRemoveAreaButton sel store redraw ref
  scrolledWindowAddWithViewport swin area
  scrolledWindowSetPolicy swin PolicyAutomatic PolicyAutomatic
  widgetAddEvents area [PointerMotionMask]
  widgetAddEvents area [PointerMotionMask]
  area `on` exposeEvent $ tryEvent $ drawViewer area vRef
  area `on` motionNotifyEvent $ tryEvent $ onMove vRef
  area `on` buttonPressEvent $ tryEvent $ onPress vRef
  area `on` enterNotifyEvent $ tryEvent $ onEnter
  area `on` buttonReleaseEvent $ tryEvent $ onRelease vRef
  mimport `on` menuItemActivate $ void $ dialogRun ifch
  msave `on` menuItemActivate $ void $ dialogRun jfch
  ifch  `on` response $ onJsonImport ref redraw store ifch
  jfch  `on` response $ onJsonSave ref jfch
  onEntryActivated (onPropEntryActivated vRef) propNameEntry
  onComboChanged (onPropComboChanged vRef) propTypeCombo
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

treeSelection :: TreeSelection -> ListStore Rect -> IO (Maybe (TreeIter, Rect))
treeSelection sel store =
    traverse go =<< treeSelectionGetSelected sel
  where
    go iter =
        let idx = listStoreIterToIndex iter in
        fmap (\r -> (iter, r)) (listStoreGetValue store idx)

selectTreeItem :: TreeSelection
               -> IORef Viewer
               -> TreeIter
               -> IO ()
selectTreeItem sel ref iter =
    treeSelectionSelectIter sel iter

_selectRectItem :: ListStore Rect -> (TreeIter -> IO ()) -> Rect -> IO ()
_selectRectItem store selectItem r = do
    let p x = (x ^. rectId) == (r ^. rectId)
    iOpt <- lookupStoreIter p store
    traverse_ selectItem iOpt

_updateRectStore :: ListStore Rect -> Rect -> TreeIter -> IO ()
_updateRectStore store r iter =
    let idx = listStoreIterToIndex iter in
    listStoreSetValue store idx r

_appendRectStore :: ListStore Rect -> IORef Viewer -> Rect -> IO Int
_appendRectStore store ref r = do
    v  <- readIORef ref
    r' <- State.evalStateT go v
    listStoreAppend store r'
  where
    go = do
        viewerBoards.boardsState += 1
        page <- use viewerCurrentPage
        id   <- use $ viewerBoards.boardsState
        let r' = r & rectId .~ id & rectName %~ (++  show id)
        viewerBoards.boardsMap.at page.traverse.boardRects.at id ?= r'
        viewerBoards.boardsSelection .= Nothing
        v <- State.get
        liftIO $ writeIORef ref v
        return r'

_viewerGetRatio :: IORef Viewer -> IO Double
_viewerGetRatio ref = fmap go (readIORef ref)
  where
    go v =
        let pId   = v ^. viewerCurrentPage
            pZ    = v ^. viewerZoom
            pages = v ^. viewerPages
            baseW = fromIntegral (v ^. viewerBaseWidth)
            page  = pages ! pId
            zoom  = zoomValues ! pZ
            w     = pageWidth page
        in (baseW * zoom) / w

_viewerGetPageRects :: IORef Viewer -> IO [Rect]
_viewerGetPageRects = fmap go . readIORef
  where
    go v =
        let pId = v ^. viewerCurrentPage in
        v ^. viewerBoards.boardsMap.at pId.traverse.boardRects.to I.elems

_viewerGetOveredRect :: IORef Viewer -> Double -> Double -> IO (Maybe Rect)
_viewerGetOveredRect ref x y = do
    rs <- _viewerGetPageRects ref
    fmap (go rs) (readIORef ref)
  where
    go rs v =
        let (First oOpt) = foldMap (First . overed) rs in oOpt

    overed r
        | isOver 1.0 x y r = Just r
        | otherwise        = Nothing

_viewerGetOveredArea :: IORef Viewer
                    -> Double
                    -> Double
                    -> Rect
                    -> IO (Maybe Area)
_viewerGetOveredArea ref x y r = return . go =<< _viewerGetRatio ref
  where
    go ratio =
        let (First aOpt) =
                foldMap (First . overed ratio) (enumFrom TOP_LEFT) in
        aOpt

    overed ratio a
        | isOver 1.0 x y (rectArea (5/ratio) r a) = Just a
        | otherwise                               = Nothing

_viewerUpdateRect :: ListStore Rect -> IORef Viewer -> Rect -> IO ()
_viewerUpdateRect store ref r = do
    writeIORef ref . State.execState go =<< readIORef ref
    iOpt <- lookupStoreIter ((== (r ^. rectId)) . _rectId) store
    traverse_ (_updateRectStore store r) iOpt
  where
    go = do
        page <- use viewerCurrentPage
        let id = r ^. rectId
        viewerBoards.boardsMap.at page.traverse.boardRects.at id ?= r
        viewerBoards.boardsEvent .= None

withRectStoreIter :: ListStore Rect -> (TreeIter -> IO r) -> Int -> IO ()
withRectStoreIter store k i =
    treeModelForeach store $ \iter ->
        if listStoreIterToIndex iter == i
        then True <$ k iter
        else return False

withRatioCoord :: IORef Viewer
               -> (Double -> Double -> IO a)
               -> Double
               -> Double
               -> IO a
withRatioCoord ref k x y = do
    ratio <- _viewerGetRatio ref
    k (x/ratio) (y/ratio)

_viewerGetEvent :: IORef Viewer -> IO BoardEvent
_viewerGetEvent ref = fmap go (readIORef ref)
  where
    go v = v ^. viewerBoards.boardsEvent

_viewerSetEvent :: IORef Viewer -> BoardEvent -> IO ()
_viewerSetEvent ref e = modifyIORef ref (State.execState go)
  where
    go = do
        viewerBoards.boardsEvent .= e
        traverse_ upd (eventGetRect e)

    upd r = do
        page <- use viewerCurrentPage
        let id = r ^. rectId
        viewerBoards.boardsMap.at page.traverse.boardRects.at id .= Nothing

_viewerModifyEvent :: IORef Viewer -> (BoardEvent -> BoardEvent) -> IO ()
_viewerModifyEvent ref k = do
    e <- _viewerGetEvent ref
    let !e' = case e of
                None -> e
                _    -> k e
    _viewerSetEvent ref e'

_viewerGetSelection :: IORef Viewer -> IO (Maybe Rect)
_viewerGetSelection ref = fmap go (readIORef ref)
  where
    go v = v ^. viewerBoards.boardsSelection

_viewerSetSelection :: IORef Viewer -> Rect -> IO ()
_viewerSetSelection ref r = modifyIORef ref go
  where
    go v = v & viewerBoards.boardsSelection ?~ r

_viewerModifySelection :: IORef Viewer -> (Rect -> Rect) -> IO ()
_viewerModifySelection ref k = do
    sOpt <- _viewerGetSelection ref
    traverse_ (_viewerSetSelection ref) (fmap k sOpt)

_viewerClearSelection :: IORef Viewer -> IO ()
_viewerClearSelection ref = modifyIORef ref go
  where
    go v = v & viewerBoards.boardsSelection .~ Nothing

_viewerLookupIter :: IORef Viewer -> (Rect -> Bool) -> IO (Maybe Rect)
_viewerLookupIter ref p = fmap go (_viewerGetPageRects ref)
  where
    go = getFirst . foldMap (First . search)

    search r
        | p r       = Just r
        | otherwise = Nothing

_viewerGetOvered :: IORef Viewer -> IO (Maybe Rect)
_viewerGetOvered = fmap go . readIORef
  where
    go v = v ^. viewerBoards.boardsOvered

_viewerSetOvered :: IORef Viewer -> Maybe Rect -> IO ()
_viewerSetOvered ref rOpt = modifyIORef ref go
  where
    go v = v & viewerBoards.boardsOvered .~ rOpt

_viewerPageItem :: IORef Viewer -> IO PageItem
_viewerPageItem = fmap go . readIORef
  where
    go v =
        let idx   = v ^. viewerCurrentPage
            pages = v ^. viewerPages in
        pages ! idx

_viewerGetSelected :: IORef Viewer -> IO (Maybe Rect)
_viewerGetSelected = fmap go . readIORef
  where
    go v = v ^. viewerBoards.boardsSelected

_viewerSetSelected :: IORef Viewer -> Maybe Rect -> IO ()
_viewerSetSelected ref rOpt = modifyIORef ref go
  where
    go v = v & viewerBoards.boardsSelected .~ rOpt

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
  --registerViewerEvents store ref
  return ref

lookupStoreIter :: (a -> Bool) -> ListStore a -> IO (Maybe TreeIter)
lookupStoreIter pred store = treeModelGetIterFirst store >>= go
    where
      go (Just it) = do
        a <- listStoreGetValue store (listStoreIterToIndex it)
        if pred a
        then return (Just it)
        else treeModelIterNext store it >>= go
      go _ = return Nothing
