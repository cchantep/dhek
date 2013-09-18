{-# LANGUAGE ScopedTypeVariables #-}
module Dhek.Action where

import Prelude hiding (foldr)
import Control.Applicative (WrappedMonad(..))
import Control.Lens
import Control.Monad (void, when, join)
import Control.Monad.State (execState, evalState, execStateT)
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad.Trans (MonadIO(..))
import Data.Array
import qualified Data.IntMap as I
import Data.IORef (IORef, newIORef, readIORef, modifyIORef, writeIORef)
import Data.Foldable (traverse_, foldMap, foldr)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Monoid (First(..), Sum(..))
import Dhek.Types
import Graphics.Rendering.Cairo
  (Render, setSourceRGB, scale, setLineWidth, rectangle, closePath, stroke, fill)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Poppler.Document
  (Page, documentNewFromFile, documentGetNPages, documentGetPage)
import Graphics.UI.Gtk.Poppler.Page

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

resizeRect :: Double -> Double -> Area -> Rect -> Rect
resizeRect dx dy area r = execState (go area) r
    where
      go TOP_LEFT = do
        rectX += dx
        rectY += dy
        rectWidth  -= dx
        rectHeight -= dy
      go TOP = do
        rectY += dy
        rectHeight -= dy
      go TOP_RIGHT = do
        rectY += dy
        rectWidth  += dx
        rectHeight -= dy
      go RIGHT = do
        rectWidth += dx
      go BOTTOM_RIGHT = do
        rectWidth += dx
        rectHeight += dy
      go BOTTOM = do
        rectHeight += dy
      go BOTTOM_LEFT = do
        rectX += dx
        rectWidth -= dx
        rectHeight += dy
      go LEFT = do
        rectX += dx
        rectWidth -= dx

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
                  (Hold r _)     -> Just r
                  (Resize r _ _) -> Just r
                  _          -> Nothing
            rectSel = v ^. viewerBoards.boardsSelection
        frame <- widgetGetDrawWindow area
        (fW, fH) <- drawableGetSize frame
        widgetSetSizeRequest area (truncate width) (truncate height)
        renderWithDrawable frame (setSourceRGB 1.0 1.0 1.0 >>
                                  rectangle 0 0 (fromIntegral fW) (fromIntegral fH) >>
                                  fill                   >>
                                  scale ratio ratio      >>
                                  pageRender page        >>
                                  drawRects th sel rects >>
                                  drawingSel rectSel     >>
                                  drawRects th Nothing evRect)

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
