module Action where

import Prelude hiding (foldr)
import Control.Applicative (WrappedMonad(..))
import Control.Monad (void)
import Control.Monad.Trans (MonadIO(..))
import Data.Maybe (fromJust)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef, writeIORef)
import Data.Foldable (traverse_, foldMap, foldr)
import Data.Monoid (First(..))
import Graphics.Rendering.Cairo
  (Render, setSourceRGB, scale, setLineWidth, rectangle, closePath, stroke)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Poppler.Document
  (Page, documentNewFromFile, documentGetNPages, documentGetPage)
import Graphics.UI.Gtk.Poppler.Page
import Types (Viewer(..), Rect(..), rectNew)


modifyCurPage :: (Int -> Int) -> Viewer -> Viewer
modifyCurPage k v =
  let cur = viewerCurrentPage v in
  v{ viewerCurrentPage = k cur }

setViewerZoom :: Double -> Viewer -> Viewer
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
            -> (Bool, Bool, Viewer) --decide which button to toggle and the new current page valueu
onNavButton k v =
  let count = viewerPageCount v
      cur   = viewerCurrentPage v
      (tPrev, tNext, newCur) = k cur count in
  (tPrev, tNext, modifyCurPage (const newCur) v)

onMove :: IORef Viewer -> EventM EMotion ()
onMove ref = do
  v     <- liftIO $ readIORef ref
  ratio <- getPageRatio ref
  rectDetection ref ratio
  updateSelection ref ratio
  liftIO $ readIORef ref >>= askDrawingViewer

onPress :: IORef Viewer -> EventM EButton ()
onPress ref = do
  (x, y) <- eventCoordinates
  ratio  <- getPageRatio ref
  liftIO $ putStrLn ("Start in " ++ show (x,y))
  let f v = v{viewerSelection= Just (rectNew (x/ratio) (y/ratio) 0 0)}
  liftIO $ modifyIORef ref f

onRelease :: IORef Viewer -> EventM EButton ()
onRelease ref =
  eventCoordinates >>= \(x,y) ->
    liftIO $ do
      v <- readIORef ref
      let selection   = viewerSelection v
          rects       = viewerRects v
          newV        = v { viewerSelection = Nothing
                          , viewerRects     = foldr (:) rects selection }
      putStrLn ("End in " ++ show (x,y))
      writeIORef ref newV
      askDrawingViewer newV

rectDetection :: IORef Viewer -> Double -> EventM EMotion ()
rectDetection ref ratio = do
  v <- liftIO $ readIORef ref
  let rects = viewerRects v
      thick = viewerThickness v
  go rects (thick / 2)
  where
    overRect thick x y r@(Rect rX rY height width _ _) =
      let adjustX = (rX + width  + thick) * ratio
          adjustY = (rY + height + thick) * ratio in

      x >= ((rX - thick) * ratio) && x <= adjustX &&
      y >= ((rY - thick) * ratio) && y <= adjustY

    go rects thick =
      eventCoordinates >>= \(x,y) ->
        let f r | overRect thick x y r = First (Just r)
                | otherwise            = First Nothing
            (First res) = foldMap f rects
            action = modifyIORef ref (\v -> v{viewerSelectedRect=res}) in
        liftIO $ action

updateSelection :: IORef Viewer -> Double -> EventM EMotion ()
updateSelection ref ratio =
  unwrapMonad . traverse_ (WrapMonad . go) =<< getSel
  where
    getSel = liftIO $ fmap viewerSelection (readIORef ref)

    go (Rect x0 y0 _ _ _ _) =
      eventCoordinates >>= \(x,y) ->
        let newHeight = (y/ratio) - y0
            newWidth  = (x/ratio) - x0
            f v = v{viewerSelection = Just (rectNew x0 y0 newHeight newWidth)} in
        liftIO $ modifyIORef ref f

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
  return (Viewer area doc swin 1 nb 1 760 [] Nothing 5.0 Nothing)

registerViewerEvents :: IORef Viewer -> IO ()
registerViewerEvents ref = do
  v <- readIORef ref
  let area = viewerArea v
  widgetAddEvents area [PointerMotionMask]
  area `on` exposeEvent $ tryEvent $ drawViewer ref
  area `on` motionNotifyEvent $ tryEvent $ onMove ref
  area `on` buttonPressEvent $ tryEvent $ onPress ref
  void $ area `on` buttonReleaseEvent $ tryEvent $ onRelease ref

getPageAndSize :: IORef Viewer -> IO (Page, Double, Double, Double)
getPageAndSize ref = do
  v <- readIORef ref
  let doc   = viewerDocument v
      cur   = viewerCurrentPage v
      baseW = viewerBaseWidth v
      zoom  = viewerZoom v
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
          area    = viewerArea v
          rects   = viewerRects v
          sel     = viewerSelectedRect v
          rectSel = viewerSelection v
      frame <- widgetGetDrawWindow area
      widgetSetSizeRequest area (truncate width) (truncate height)
      renderWithDrawable frame (setSourceRGB 1.0 1.0 1.0 >>
                                scale ratio ratio        >>
                                pageRender page          >>
                                --pushGroup                >>
                                drawRects th sel rects >>
                                drawingSel rectSel) -- >>
                                --popGroupToSource)

    drawRects th sel = unwrapMonad . traverse_ (WrapMonad . drawing th sel)

    drawing :: Double -> Maybe Rect -> Rect -> Render ()
    drawing th sel r@(Rect x y h w _ _) =
      let step (Just s)
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
        go r@(Rect x y h w _ _) =
          do  setSourceRGB 0 1.0 0
              setLineWidth 1
              rectangle x y w h
              closePath
              stroke
