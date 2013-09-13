{-# LANGUAGE ScopedTypeVariables #-}
module Action where

import Prelude hiding (foldr)
import Control.Applicative (WrappedMonad(..))
import Control.Lens
import Control.Monad (void, when, join)
import Control.Monad.State (execState, evalState)
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad.Trans (MonadIO(..))
import Data.Array
import qualified Data.IntMap as I
import Data.IORef (IORef, newIORef, readIORef, modifyIORef, writeIORef)
import Data.Foldable (traverse_, foldMap, foldr)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Monoid (First(..), Sum(..))
import Graphics.Rendering.Cairo
  (Render, setSourceRGB, scale, setLineWidth, rectangle, closePath, stroke, fill)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Poppler.Document
  (Page, documentNewFromFile, documentGetNPages, documentGetPage)
import Graphics.UI.Gtk.Poppler.Page
import Types

zoomValues :: Array Int Double
zoomValues = array (0, 10) values
  where
    values = [(0,  0.125) -- 12.5%
             ,(1,  0.25)  -- 25%
             ,(2,  0.5)   -- 50%
             ,(3,  1.0)   -- 100%
             ,(4,  2.0)   -- 200%
             ,(5,  3.0)   -- 300%
             ,(6,  4.0)   -- 400%
             ,(7,  5.0)   -- 500%
             ,(8,  6.0)   -- 600%
             ,(9,  7.0)   -- 700%
             ,(10, 8.0)]  -- 800%

askDrawingViewer :: Viewer -> IO ()
askDrawingViewer v =
  let page = v ^. viewerCurrentPage
      area = v ^. viewerBoards.boardsArea in
  widgetQueueDraw area

onPrevState :: Int -> Int -> (Bool, Bool, Int)
onPrevState cur count =
  let newCur = cur - 1 in (newCur - 1 < 1, cur == count, newCur)

onNextState :: Int -> Int -> (Bool, Bool, Int)
onNextState cur count =
  let newCur = cur + 1 in (newCur + 1 > count, cur == 1, newCur)

onNavButton :: (Int -> Int -> (Bool, Bool, Int))
            -> Viewer
            -> (Bool, Bool, Viewer) --decide which button to toggle and the new current page value
onNavButton k v =
  let count = v ^. viewerPageCount
      cur   = v ^. viewerCurrentPage
      (tPrev, tNext, newCur) = k cur count in
  (tPrev, tNext, v & viewerCurrentPage .~ newCur)

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

            action = do
              sel'   <- use (viewerBoards.boardsSelected.traverse.to Sum)
              page   <- use viewerCurrentPage
              board  <- use (viewerBoards.boardsMap.at page.traverse)
              select <- use (viewerBoards.boardsSelection)
              let sel      = getSum sel'
                  rOpt     = board ^. boardRects.at sel
                  toHold r = Hold r (xR, yR)
                  hOpt     = fmap toHold rOpt
              maybe onNoSel (onHold board page sel) hOpt

        liftIO $ writeIORef ref (execState action v)

onRelease :: IORef Viewer -> EventM EButton ()
onRelease ref = do
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
                  let x' = x & rectId .~ id & rectName %~ (++ show id)
                  board.boardRects.at id ?= x'

                onHold (Hold r _) = do
                  board <- use (viewerBoards.boardsMap.at page.traverse)
                  let rId = r ^. rectId
                  viewerBoards.boardsMap.at page.traverse .= (board & boardRects.at rId .~ (Just r))
                onHold _ = return ()

                action = do
                  selection <- use (viewerBoards.boardsSelection)
                  event     <- use (viewerBoards.boardsEvent)
                  viewerBoards.boardsSelection .= Nothing
                  viewerBoards.boardsEvent .= None
                  traverse_ insert selection
                  onHold event
                newV = execState action v
            putStrLn ("End in " ++ show (x,y))
            writeIORef ref newV
            askDrawingViewer newV

rectDetection :: Viewer -> Double -> EventM EMotion (Maybe Int)
rectDetection v ratio = do
  let page   = v ^. viewerCurrentPage
      board  = viewerBoards.boardsMap.at page.traverse
      rects' =  v ^. board.boardRects
      rects  = I.elems rects'
      thick  = v ^. viewerBoards.boardsThick
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

getPageRatio :: MonadIO m => IORef Viewer -> m Double
getPageRatio = liftIO . fmap (\(_,r,_,_) -> r) . getPageAndSize

loadPdf :: FilePath -> IO Viewer
loadPdf path = do
  area <- drawingAreaNew
  doc  <- fmap fromJust (documentNewFromFile path Nothing)
  swin <- scrolledWindowNew Nothing Nothing
  nb   <- documentGetNPages doc
  scrolledWindowAddWithViewport swin area
  scrolledWindowSetPolicy swin PolicyAutomatic PolicyAutomatic
  return (Viewer doc 1 nb (boardsNew nb area swin 777 3 1.0))

registerViewerEvents :: IORef Viewer -> IO ()
registerViewerEvents ref = do
  v <- readIORef ref
  let area = v ^. viewerBoards.boardsArea
  widgetAddEvents area [PointerMotionMask]
  area `on` exposeEvent $ tryEvent $ drawViewer ref
  area `on` motionNotifyEvent $ tryEvent $ onMove ref
  area `on` buttonPressEvent $ tryEvent $ onPress ref
  area `on` enterNotifyEvent $ tryEvent $ onEnter
  void $ area `on` buttonReleaseEvent $ tryEvent $ onRelease ref
    where
      onEnter = do
        frame  <- eventWindow
        cursor <- liftIO $ cursorNew Tcross
        liftIO $ drawWindowSetCursor frame (Just cursor)

getPageAndSize :: IORef Viewer -> IO (Page, Double, Double, Double)
getPageAndSize ref = do
  v <- readIORef ref
  let doc   = v ^. viewerDocument
      cur   = v ^. viewerCurrentPage
      baseW = v ^. viewerBoards.boardsBaseWidth
      idx   = v ^. viewerBoards.boardsZoom
      zoom  = zoomValues ! idx
  page <- documentGetPage doc (cur - 1)
  (width, height) <- pageGetSize page
  let rWidth = (fromIntegral baseW) * zoom
      ratio  = rWidth / width
  return (page, ratio, rWidth, ratio * height)

drawViewer :: IORef Viewer -> EventM EExpose ()
drawViewer = liftIO . go
  where
    go ref = do
      v <- readIORef ref
      (page, ratio, width, height) <- getPageAndSize ref
      let th      = v ^. viewerBoards.boardsThick
          pageId  = v ^. viewerCurrentPage
          area    = v ^. viewerBoards.boardsArea
          rects'  = v ^. viewerBoards.boardsMap.at pageId.traverse.boardRects
          rects   = I.elems rects'
          sel' = v ^. viewerBoards.boardsSelected
          rmap = v ^. viewerBoards.boardsMap.at pageId.traverse.boardRects
          sel = (\i -> I.lookup i rmap) =<< sel'
          event = v ^. viewerBoards.boardsEvent
          evRect =
            case event of
              (Hold r _) -> Just r
              _          -> Nothing
          --v ^. viewerBoards.boardsMap.at page.traverse.boardRects.at sel'
          --sf idx  = use (viewerBoards.boardsMap.at page.traverse.boardRects.at idx)
          --sel     = evalState (traverse sf sel') v
          rectSel = v ^. viewerBoards.boardsSelection
      frame <- widgetGetDrawWindow area
      (fW, fH) <- drawableGetSize frame
      widgetSetSizeRequest area (truncate width) (truncate height)
      renderWithDrawable frame (setSourceRGB 1.0 1.0 1.0 >>
                                --setLineWidth 10 >>
                                rectangle 0 0 (fromIntegral fW) (fromIntegral fH) >>
                                fill >>
                                --closePath >>
                                --stroke >>
                                scale ratio ratio        >>
                                pageRender page          >>
                                --pushGroup                >>
                                drawRects th sel rects >>
                                drawingSel rectSel >>
                                drawRects th Nothing evRect) -- >>
                                --popGroupToSource)

    drawRects th sel =
      unwrapMonad . traverse_ (WrapMonad . drawing th sel)

    drawing :: Double -> Maybe Rect -> Rect -> Render ()
    drawing th sel r =
      let x = r ^. rectX
          y = r ^. rectY
          h = r ^. rectHeight
          w = r ^. rectWidth
          step (Just s)
            | s == r    = setSourceRGB 1.0 0 0
            | otherwise = setSourceRGB 0 0 1.0
          step _ = setSourceRGB 0 0 1.0 in
      do step sel
         setLineWidth th
         rectangle x y w h
         closePath
         stroke

    drawingSel = unwrapMonad . traverse_ (WrapMonad . go)
      where
        go r =
          let x = r ^. rectX
              y = r ^. rectY
              h = r ^. rectHeight
              w = r ^. rectWidth in
          do  setSourceRGB 0 1.0 0
              setLineWidth 1
              rectangle x y w h
              closePath
              stroke
