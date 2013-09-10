module Action where

import Prelude hiding (foldr)
import Control.Applicative (WrappedMonad(..))
import Control.Monad (void, when, join)
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad.Trans (MonadIO(..))
import Data.Array
import qualified Data.IntMap as I
import Data.IORef (IORef, newIORef, readIORef, modifyIORef, writeIORef)
import Data.Foldable (traverse_, foldMap, foldr)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Monoid (First(..))
import Data.Traversable (traverse)
import Graphics.Rendering.Cairo
  (Render, setSourceRGB, scale, setLineWidth, rectangle, closePath, stroke, fill)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Poppler.Document
  (Page, documentNewFromFile, documentGetNPages, documentGetPage)
import Graphics.UI.Gtk.Poppler.Page
import Types
  (Viewer(..), Rect(..), RectStore(..),
   rectNew, addRect, emptyStore, normalize)

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

modifyCurPage :: (Int -> Int) -> Viewer -> Viewer
modifyCurPage k v =
  let cur = viewerCurrentPage v in
  v{ viewerCurrentPage = k cur }

setViewerZoom :: Int -> Viewer -> Viewer
setViewerZoom z v = v{ viewerZoom = z }

updatePageSpinValue :: SpinButton -> Viewer -> IO ()
updatePageSpinValue spin v =
  let cur = viewerCurrentPage v in
  spinButtonSetValue spin (fromIntegral cur)

askDrawingViewer :: Viewer -> IO ()
askDrawingViewer v =
  let area = viewerArea v in
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
  let count = viewerPageCount v
      cur   = viewerCurrentPage v
      (tPrev, tNext, newCur) = k cur count in
  (tPrev, tNext, modifyCurPage (const newCur) v)

onMove :: IORef Viewer -> EventM EMotion ()
onMove ref = do
  frame <- eventWindow
  v     <- liftIO $ readIORef ref
  ratio <- getPageRatio ref
  dopt  <- rectDetection v ratio
  sopt  <- updateSelection v ratio
  let v1 = v { viewerSelectedRect = dopt }
      v2 = foldr (\r v -> v {viewerSelection = Just r}) v1 sopt

      changed = viewerSelection v /= viewerSelection v2
                || viewerSelectedRect v /= viewerSelectedRect v2

      cursor
        | isJust dopt && isNothing sopt = Hand1
        | otherwise                     = Tcross

      updateCursor =
        drawWindowSetCursor frame . Just =<< cursorNew cursor

  liftIO $ do
    when changed updateCursor
    when changed (writeIORef ref v2)
    when changed (askDrawingViewer v2)

onPress :: IORef Viewer -> EventM EButton ()
onPress ref = do
  b <- eventButton
  when (b == LeftButton) go
    where
      go = do
        (x, y) <- eventCoordinates
        ratio  <- getPageRatio ref
        liftIO $ putStrLn ("Start in " ++ show (x,y))
        let f v = v{viewerSelection= Just (rectNew (x/ratio) (y/ratio) 0 0)}
        liftIO $ modifyIORef ref f

onRelease :: IORef Viewer -> EventM EButton ()
onRelease ref = do
  b <- eventButton
  when (b == LeftButton) go
    where
      go =
        eventCoordinates >>= \(x,y) ->
          liftIO $ do
            v <- readIORef ref
            let select = viewerSelection v
                store  = viewerStore v
                page   = (viewerCurrentPage v) - 1
                insert = addRect page . normalize
                newV   = v { viewerSelection = Nothing
                           , viewerStore     = foldr insert store select }
            putStrLn ("End in " ++ show (x,y))
            writeIORef ref newV
            askDrawingViewer newV

rectDetection :: Viewer -> Double -> EventM EMotion (Maybe Rect)
rectDetection v ratio = do
  let page  = (viewerCurrentPage v) - 1
      rects = I.lookup page (rstoreRects $ viewerStore v)
      thick = viewerThickness v
  fmap join $ traverse (go (thick / 2)) rects
  where
    overRect thick x y r@(Rect rX rY height width _ _) =
      let adjustX = (rX + width  + thick) * ratio
          adjustY = (rY + height + thick) * ratio in

      x >= ((rX - thick) * ratio) && x <= adjustX &&
      y >= ((rY - thick) * ratio) && y <= adjustY

    go thick rects =
      eventCoordinates >>= \(x,y) ->
        let f r | overRect thick x y r = First (Just r)
                | otherwise            = First Nothing
            (First res) = foldMap f rects in
        return res

updateSelection :: Viewer -> Double -> EventM EMotion (Maybe Rect)
updateSelection v ratio =
  traverse go opt
  where
    opt = viewerSelection  v
    go r =
      eventCoordinates >>= \(x,y) ->
        let x0        = rectX r
            y0        = rectY r
            newHeight = (y/ratio) - y0
            newWidth  = (x/ratio) - x0
            newR      = r { rectHeight = newHeight
                          , rectWidth  = newWidth } in
        return newR

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
  return (Viewer area doc swin 1 nb 3 777 emptyStore Nothing 1.0 Nothing)

registerViewerEvents :: IORef Viewer -> IO ()
registerViewerEvents ref = do
  v <- readIORef ref
  let area = viewerArea v
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
  let doc   = viewerDocument v
      cur   = viewerCurrentPage v
      baseW = viewerBaseWidth v
      idx   = viewerZoom v
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
      let th      = viewerThickness v
          pageIdx = (viewerCurrentPage v) - 1
          area    = viewerArea v
          rects   = I.lookup pageIdx (rstoreRects $ viewerStore v)
          sel     = viewerSelectedRect v
          rectSel = viewerSelection v
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
                                drawingSel rectSel) -- >>
                                --popGroupToSource)

    drawRects th sel =
      unwrapMonad . traverse_ (traverse_ (WrapMonad . drawing th sel))

    drawing :: Double -> Maybe Rect -> Rect -> Render ()
    drawing th sel r =
      let x = rectX r
          y = rectY r
          h = rectHeight r
          w = rectWidth r
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
          let x = rectX r
              y = rectY r
              h = rectHeight r
              w = rectWidth r in
          do  setSourceRGB 0 1.0 0
              setLineWidth 1
              rectangle x y w h
              closePath
              stroke
