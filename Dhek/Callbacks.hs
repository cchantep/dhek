{-# LANGUAGE DoAndIfThenElse #-}
module Dhek.Callbacks where

import Prelude hiding (foldr)
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad (when, void, join)
import Control.Monad.State (execState, evalState, execStateT)
import Control.Monad.Trans (liftIO)
import Data.Aeson (encode, eitherDecode)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef, writeIORef)
import Data.Foldable (traverse_, foldMap, foldr)
import Data.Functor ((<$))
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap as I
import Data.List (dropWhileEnd)
import Dhek.Action
import Dhek.Types hiding (ViewerRef(..))
import Dhek.Utils
import Dhek.ViewerRef
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Monoid (First(..), Sum(..))
import Graphics.UI.Gtk

onNavCommon :: String
            -> (Int -> Int)
            -> Button
            -> Button
            -> ViewerRef
            -> IO ()
onNavCommon name k prev next v=
    viewerModifyCurPage v $ \cur -> do
        rects <- viewerGetPageRects v
        nb    <- viewerPageNb v
        viewerSetRects v rects
        let new   = k cur
            title = name ++ " (page " ++ show new ++" / " ++ show nb ++ ")"
        windowSetTitle (viewerRefWindow v) title
        widgetSetSensitive prev (new /= 1)
        widgetSetSensitive next (new /= nb)
        return new

onJsonSave :: IORef Viewer
           -> FileChooserDialog
           -> ResponseId
           -> IO ()
onJsonSave _ jfch ResponseCancel = widgetHide jfch
onJsonSave ref jfch ResponseOk =
    readIORef ref >>= \v ->
        let nb          = v ^. viewerPageCount
            tup (i, b)  = (i, b ^. boardRects.to I.elems)
            toList      = fmap tup . I.toList
            rects       = v ^. viewerBoards.boardsMap.to toList
            save        = saveNew $ fillUp nb rects
            ensure path
                | takeExtension path == ".json" = path
                | otherwise                     = path ++ ".json"
            write path  = B.writeFile (ensure path) (encode save) in
        do opt <- fileChooserGetFilename jfch
           traverse_ write opt
           widgetHide jfch

onJsonImport :: IORef Viewer
             -> IO ()
             -> ListStore Rect
             -> FileChooserDialog
             -> ResponseId
             -> IO ()
onJsonImport _ _ _ ifch ResponseCancel = widgetHide ifch
onJsonImport ref redraw store ifch ResponseOk = do
  opt  <- fileChooserGetFilename ifch
  bOpt <- traverse go opt
  traverse_ (traverse_ updViewer) bOpt
    where
      go path = do
        bytes <- B.readFile path
        let boardsE = fmap saveToBoards (eitherDecode bytes)
        either showError (return . Just) boardsE

      showError e = do
        p <- windowGetTransientFor ifch
        m <- messageDialogNew p [DialogModal] MessageError ButtonsOk e
        dialogRun m
        widgetHide m
        return Nothing

      updViewer boards = do
        v <- readIORef ref
        let v'    = v & viewerBoards .~ boards
            page  = v ^. viewerCurrentPage
            rects = boards ^. boardsMap.at page.traverse.boardRects.to I.elems
        writeIORef ref v'
        listStoreClear store
        traverse_ (listStoreAppend store) rects
        widgetHide ifch
        redraw

onCommonScale :: (Int -> Int)
              -> Button -- minus button
              -> Button -- plus button
              -> ViewerRef
              -> IO ()
onCommonScale k minus plus ref =
    viewerModifyCurZoom ref $ \cur ->
        let new = k cur in
        do widgetSetSensitive minus (new /= 0)
           widgetSetSensitive plus (new /= 10)
           return new

onTreeSelection :: ViewerRef -> IO ()
onTreeSelection ref = do
    rOpt <- fmap (fmap snd) (viewerGetTreeSelection ref)
    viewerSetSelected ref rOpt
    viewerDraw ref

onRemoveArea :: TreeSelection
             -> ListStore Rect
             -> IO ()
             -> IORef Viewer
             -> IO ()
onRemoveArea sel store redraw ref = do
  v   <- readIORef ref
  opt <- treeSelectionGetSelected sel
  traverse_ (delete v) opt
    where
      delete v i =
          let idx   = listStoreIterToIndex i
              page  = v ^. viewerCurrentPage
              board = viewerBoards.boardsMap.at page.traverse.boardRects in
          do r <- listStoreGetValue store idx
             let id = r ^. rectId
                 v' = v & board.at id .~ Nothing
             listStoreRemove store idx
             writeIORef ref v'
             redraw

type PdfCallback = FileChooserDialog
                 -> MenuItem -- Import item
                 -> MenuItem -- Export item
                 -> Window
                 -> IO HBox

openPdfFileChooser :: PdfCallback
                   -> VBox
                   -> FileChooserDialog
                   -> Window
                   -> MenuItem
                   -> MenuItem
                   -> MenuItem
                   -> IO ()
openPdfFileChooser k vbox dialog win mopen mimport msave = do
  resp <- dialogRun dialog
  widgetHide dialog
  case resp of
    ResponseCancel -> return ()
    ResponseOk     -> do
      avbox <- alignmentNew 0 0 1 1
      vvbox <- k dialog mimport msave win
      containerAdd avbox vvbox
      boxPackStart vbox avbox PackGrow 0
      widgetSetSensitive mopen False
      widgetShowAll avbox

onMove :: ViewerRef -> EventM EMotion ()
onMove ref = do
    frame   <- eventWindow
    (x',y') <- eventCoordinates
    liftIO $ do
        ratio <- viewerGetRatio ref
        let (x,y) = (x'/ratio, y'/ratio)
        viewerReportPosition ref x y
        oOpt <- viewerGetOvered ref
        dOpt <- viewerGetOveredRect ref x y
        aOpt <- fmap join (traverse (viewerGetOveredArea ref x y) dOpt)
        viewerSetOvered ref dOpt
        viewerModifySelection ref (updateSelection x y)
        viewerModifyEvent ref (updateEvent x y)
        sOpt <- viewerGetSelection ref
        evt  <- viewerGetEvent ref
        let onEvent     = isJust $ eventGetRect evt
            onSelection = isJust sOpt
            changed     = (oOpt /= dOpt) || onEvent || onSelection

            areaCursor TOP_LEFT     = TopLeftCorner
            areaCursor TOP          = TopSide
            areaCursor TOP_RIGHT    = TopRightCorner
            areaCursor RIGHT        = RightSide
            areaCursor BOTTOM_RIGHT = BottomRightCorner
            areaCursor BOTTOM       = BottomSide
            areaCursor BOTTOM_LEFT  = BottomLeftCorner
            areaCursor LEFT         = LeftSide

            eventCursor (Hold _ _)     = Hand1
            eventCursor (Resize _ _ a) = areaCursor a
            eventCursor _              = Tcross

            onArea      = isJust aOpt
            noSelection = not onSelection
            cursor =
                case () of
                  _ | onArea && noSelection      -> areaCursor $ fromJust aOpt
                    | onEvent                    -> eventCursor evt
                    | isJust dOpt && noSelection -> Hand1
                    | otherwise                  -> Tcross

        c <- cursorNew cursor
        drawWindowSetCursor frame (Just c)
        when changed $ viewerDraw ref

onPress :: ViewerRef -> EventM EButton ()
onPress ref = do
    b <- eventButton
    c <- eventClick
    when (b == LeftButton && c == SingleClick) go
  where
    go = do
        (x', y') <- eventCoordinates
        liftIO $ do
             ratio <- viewerGetRatio ref
             let (x,y)   = (x'/ratio, y'/ratio)
                 sel     = rectNew x y 0 0
                 onEvt r = do
                     aOpt <- viewerGetOveredArea ref x y r
                     let evt = maybe (Hold r (x,y)) (Resize r (x,y)) aOpt
                     viewerSetEvent ref evt
             oOpt <- viewerGetOveredRect ref x y
             maybe (viewerSetSelection ref sel) onEvt oOpt

onRelease :: ViewerRef -> EventM EButton ()
onRelease ref = do
    b <- eventButton
    when (b == LeftButton) (liftIO go)
  where
    go = do
        evt <- viewerGetEvent ref
        sel <- viewerGetSelection ref
        traverse_ (insert . normalize) sel
        traverse_ (upd . normalize) (eventGetRect evt)

    upd r = do
        viewerSetRect ref r
        viewerSelectRect ref r

    insert x =
        let w  = x ^. rectWidth
            h  = x ^. rectHeight in
        if (w*h >= 30)
        then viewerInsertRect ref x
        else viewerClearSelection ref

updateSelection :: Double -> Double -> Rect -> Rect
updateSelection x y = execState go
  where
    go = do
        x0 <- use rectX
        y0 <- use rectY
        rectWidth  .= x - x0
        rectHeight .= y - y0

updateEvent :: Double -> Double -> BoardEvent -> BoardEvent
updateEvent x y e =
    case e of
      Hold r (x0,y0)     -> Hold (translateRect (x-x0) (y-y0) r) (x,y)
      Resize r (x0,y0) a -> Resize (resizeRect (x-x0) (y-y0) a r) (x,y) a

onPropAreaSelection :: Entry -> ListStore String -> ComboBox -> Rect -> IO ()
onPropAreaSelection entry store combo r = do
  entrySetText entry (r ^. rectName)
  let pred x = x == (r ^. rectType)
  opt <- _lookupStoreIter pred store
  traverse_ (comboBoxSetActiveIter combo) opt

onPropClear :: Entry -> ComboBox -> IO ()
onPropClear entry combo = do
  entrySetText entry ""
  comboBoxSetActive combo (negate 1)

onPropEntryActivated :: ViewerRef -> String -> IO ()
onPropEntryActivated ref name =
    traverse_ go =<< viewerGetSelected ref
  where
    go r = do
        rOpt <- viewerLookupIter ref ((== name) . _rectName)
        sOpt <- viewerGetSelected ref
        let exist = isJust rOpt
            r'    = r & rectName .~ name
        when (not exist) (viewerSetRect ref r')
        when exist (showError ("\"" ++ name ++ "\" is already used"))

    showError e = do
        let win = viewerRefWindow ref
        m <- messageDialogNew (Just win) [DialogModal] MessageError ButtonsOk e
        dialogRun m
        widgetHide m

onPropComboChanged :: ViewerRef -> String -> IO ()
onPropComboChanged ref typ = traverse_ go =<< viewerGetSelected ref
  where
    go r =
        let r' = r & rectType .~ typ in
        viewerSetRect ref r'

_lookupStoreIter :: (a -> Bool) -> ListStore a -> IO (Maybe TreeIter)
_lookupStoreIter pred store = treeModelGetIterFirst store >>= go
    where
      go (Just it) = do
        a <- listStoreGetValue store (listStoreIterToIndex it)
        if pred a
        then return (Just it)
        else treeModelIterNext store it >>= go
      go _ = return Nothing
