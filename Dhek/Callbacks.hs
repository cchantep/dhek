module Dhek.Callbacks where

import Prelude hiding (foldr)
import Control.Lens
import Control.Monad (when, void)
import Control.Monad.State (execState, evalState, execStateT)
import Control.Monad.Trans (liftIO)
import Data.Aeson (encode, eitherDecode)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef, writeIORef)
import Data.Foldable (traverse_, foldMap, foldr)
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap as I
import Dhek.Action
import Dhek.Types
import Dhek.Utils
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Monoid (First(..), Sum(..))
import Graphics.UI.Gtk

onPrevious :: String -- pdf filename
           -> Window
           -> Button -- prev button
           -> Button -- next button
           -> ListStore Rect
           -> IORef Viewer
           -> IO ()
onPrevious name win = onNavCommon name win onPrevState

onNext :: String -- pdf filename
       -> Window
       -> Button -- next button
       -> Button -- prev button
       -> ListStore Rect
       -> IORef Viewer
       -> IO ()
onNext name win = onNavCommon name win onNextState

onNavCommon :: String
            -> Window
            -> (Int -> Int -> (Bool, Bool, Int))
            -> Button
            -> Button
            -> ListStore Rect
            -> IORef Viewer
            -> IO ()
onNavCommon name win upd self other store ref =
    readIORef ref >>= \v ->
        let nb = v ^. viewerPageCount
            (tSelf, tOther, v') = onNavButton upd v
            cur = v' ^. viewerCurrentPage
            title = name ++ " (page " ++ show cur ++" / " ++ show nb ++ ")"
            board = viewerBoards.boardsMap.at cur.traverse.boardRects
            rects = v ^. board.to I.elems in
        do listStoreClear store
           traverse_ (listStoreAppend store) rects
           widgetSetSensitive self (not tSelf)
           when tOther (widgetSetSensitive other True)
           writeIORef ref v'
           windowSetTitle win title
           askDrawingViewer v'

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
             -> ListStore Rect
             -> FileChooserDialog
             -> ResponseId
             -> IO ()
onJsonImport _ _ ifch ResponseCancel = widgetHide ifch
onJsonImport ref store ifch ResponseOk = do
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
        askDrawingViewer v'

onCommonScale :: (Int -> Int)
              -> Button -- minus button
              -> Button -- plus button
              -> IORef Viewer
              -> IO ()
onCommonScale upd minus plus ref =
    readIORef ref >>= \v ->
        let z   = v ^. viewerZoom.to upd
            low = (z-1) < 0
            up  = (z+1) > 10
            v'  = v & viewerZoom .~ z in
        do widgetSetSensitive minus (not low)
           widgetSetSensitive plus (not up)
           writeIORef ref v'
           askDrawingViewer v'

onTreeSelection :: TreeSelection -> ListStore Rect -> IORef Viewer -> IO ()
onTreeSelection sel store ref = do
  opt  <- treeSelectionGetSelected sel
  rOpt <- traverse (listStoreGetValue store . listStoreIterToIndex) opt
  v    <- readIORef ref
  let v' = v & viewerBoards.boardsSelected .~ (fmap _rectId rOpt)
  writeIORef ref v'
  askDrawingViewer v'

onRemoveArea :: TreeSelection -> ListStore Rect -> IORef Viewer -> IO ()
onRemoveArea sel store ref = do
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
             askDrawingViewer v'

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

onMove :: IORef Viewer -> EventM EMotion ()
onMove ref = do
  frame <- eventWindow
  v     <- liftIO $ readIORef ref
  ratio <- getPageRatio ref
  dopt  <- rectDetection v ratio
  sopt  <- updateSelection v ratio
  (x, y) <- eventCoordinates
  let page = v ^. viewerCurrentPage
      detL  = viewerBoards.boardsSelected
      selL  = viewerBoards.boardsSelection
      v1 = v & detL .~ dopt
      v2 = foldr (\r v -> v & selL ?~ r) v1 sopt
      event = v ^. viewerBoards.boardsEvent
      (xR, yR) = (x/ratio, y/ratio)
      newE =
        case event of
          (Hold rect (x0, y0)) ->
            Hold (translateRect (xR-x0) (yR-y0) rect) (xR,yR)
          (Resize rect  (x0, y0) area) ->
            Resize (resizeRect (xR-x0) (yR-y0) area rect) (xR, yR) area
          e -> e
      v3 = v2 & viewerBoards.boardsEvent .~ newE

      changed = (v ^. viewerBoards.boardsSelection) /= (v3 ^. viewerBoards.boardsSelection) ||
                (v ^. viewerBoards.boardsSelected) /= (v3 ^. viewerBoards.boardsSelected) ||
                (v ^. viewerBoards.boardsEvent) /= (v3 ^. viewerBoards.boardsEvent)

      cursor
          | isJust dopt && isNothing sopt = Hand1
          | otherwise                     = Tcross

      updateCursor =
        drawWindowSetCursor frame . Just =<< cursorNew cursor

  liftIO $ do
    when changed updateCursor
    when changed (writeIORef ref v3)
    when changed (askDrawingViewer v3)

onPress :: IORef Viewer -> EventM EButton ()
onPress ref = do
  b <- eventButton
  when (b == LeftButton) go
    where
      go = do
        (x, y) <- eventCoordinates
        ratio  <- getPageRatio ref
        v      <- liftIO $ readIORef ref
        let xR = x / ratio
            yR = y / ratio

            onNoSel = viewerBoards.boardsSelection ?= rectNew xR yR 0 0

            onHold board page sel h = do
              viewerBoards.boardsEvent .= h
              let board' = board & boardRects.at sel .~ Nothing
              viewerBoards.boardsMap.at page.traverse .= board'

            toEvent th r =
              let areas  = enumFrom TOP_LEFT
                  pred a = if isOver ratio th xR yR (rectArea 20 r a)
                           then Just a
                           else Nothing
                  (First aOpt) = foldMap (First . pred) areas in
              maybe (Hold r (xR, yR)) (Resize r (xR, yR)) aOpt

            action = do
              sel'   <- use (viewerBoards.boardsSelected.traverse.to Sum)
              page   <- use viewerCurrentPage
              thick  <- use viewerThick
              board  <- use (viewerBoards.boardsMap.at page.traverse)
              select <- use (viewerBoards.boardsSelection)
              let sel  = getSum sel'
                  rOpt = board ^. boardRects.at sel
                  hOpt = fmap (toEvent thick) rOpt
              maybe onNoSel (onHold board page sel) hOpt

        liftIO $ writeIORef ref (execState action v)

onRelease :: ListStore Rect -> IORef Viewer -> EventM EButton ()
onRelease store ref = do
  b <- eventButton
  when (b == LeftButton) go
    where
      go =
        eventCoordinates >>= \(x,y) ->
          liftIO $ do
            v <- readIORef ref
            let page = v ^. viewerCurrentPage
                board = viewerBoards.boardsMap.at page.traverse
                insert x = do
                  viewerBoards.boardsState += 1
                  id <- use (viewerBoards.boardsState)
                  let x'  = x & rectId .~ id & rectName %~ (++ show id)
                      x'' = normalize x'
                  liftIO $ listStoreAppend store x''
                  board.boardRects.at id ?= x''

                onHold r = do
                  board <- use (viewerBoards.boardsMap.at page.traverse)
                  let rId = r ^. rectId
                      r'  = normalize r
                  viewerBoards.boardsMap.at page.traverse .= (board & boardRects.at rId .~ (Just r'))

                action = do
                  selection <- use (viewerBoards.boardsSelection)
                  event     <- use (viewerBoards.boardsEvent)
                  viewerBoards.boardsSelection .= Nothing
                  viewerBoards.boardsEvent .= None
                  traverse_ insert selection
                  traverse_ onHold (eventGetRect event)
            newV <- execStateT action v
            putStrLn ("End in " ++ show (x,y))
            writeIORef ref newV
            askDrawingViewer newV

rectDetection :: Viewer -> Double -> EventM EMotion (Maybe Int)
rectDetection v ratio = do
  let page   = v ^. viewerCurrentPage
      board  = viewerBoards.boardsMap.at page.traverse
      rects' =  v ^. board.boardRects
      rects  = I.elems rects'
      thick  = v ^. viewerThick
  go (thick / 2) rects
    where
      overRect thick x y r@(Rect _ rX rY height width _ _) =
          let adjustX = (rX + width  + thick) * ratio
              adjustY = (rY + height + thick) * ratio in

          x >= ((rX - thick) * ratio) && x <= adjustX &&
          y >= ((rY - thick) * ratio) && y <= adjustY

      go thick rects =
          eventCoordinates >>= \(x,y) ->
              let f r | overRect thick x y r = First (Just (r ^. rectId))
                      | otherwise            = First Nothing
                  (First res) = foldMap f rects in
              return res

updateSelection :: Viewer -> Double -> EventM EMotion (Maybe Rect)
updateSelection v ratio = go
    where
      go =
          eventCoordinates >>= \(x,y) ->
              let action = do
                    opt <- use (viewerBoards.boardsSelection)
                    traverse_ upd opt
                    use (viewerBoards.boardsSelection)
                  upd r =
                      let x0   = r ^. rectX
                          y0   = r ^. rectY
                          newH = (y/ratio) - y0
                          newW = (x/ratio) - x0
                          newR = r & rectHeight .~ newH & rectWidth .~ newW in
                      viewerBoards.boardsSelection ?= newR in
              return $ evalState action v

registerViewerEvents :: ListStore Rect -> IORef Viewer -> IO ()
registerViewerEvents store ref = do
  v <- readIORef ref
  let area = v ^. viewerArea
  widgetAddEvents area [PointerMotionMask]
  area `on` exposeEvent $ tryEvent $ drawViewer ref
  area `on` motionNotifyEvent $ tryEvent $ onMove ref
  area `on` buttonPressEvent $ tryEvent $ onPress ref
  area `on` enterNotifyEvent $ tryEvent $ onEnter
  void $ area `on` buttonReleaseEvent $ tryEvent $ onRelease store ref
    where
      onEnter = do
        frame  <- eventWindow
        cursor <- liftIO $ cursorNew Tcross
        liftIO $ drawWindowSetCursor frame (Just cursor)
