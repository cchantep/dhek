module Dhek.Action where

import Prelude hiding (foldr, mapM_)

import Control.Lens
import Control.Monad.State (execState, evalState, execStateT)
import Control.Monad.Trans (liftIO)

import Data.Array (Array, array)
import qualified Data.IntMap as I
import Data.Foldable (mapM_, traverse_)
import Data.Maybe (fromJust)

import Dhek.Types
import Dhek.ViewerRef

import qualified Graphics.Rendering.Cairo         as Cairo
import qualified Graphics.UI.Gtk                  as Gtk
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import qualified Graphics.UI.Gtk.Poppler.Page     as Poppler

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

saveToBoards :: Save -> Boards
saveToBoards (Save _ xs) = execState (traverse_ go xs) (boardsNew nb)
  where
    nb = length xs

    go (page, rects) = traverse_ (traverse_ (insert page)) rects

    insert page r = do
        boardsState += 1
        id <- use boardsState
        let r' = r & rectId .~ id
        boardsMap.at page.traverse.boardRects.at id ?= r'

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

loadPdf :: FilePath -> IO Viewer
loadPdf path = do
  doc   <- fmap fromJust (Poppler.documentNewFromFile path Nothing)
  nb    <- Poppler.documentGetNPages doc
  pages <- loadPages doc
  return (Viewer doc pages 1 nb 777 3 1.0 (boardsNew nb))

loadPages :: Poppler.Document -> IO (Array Int PageItem)
loadPages doc = do
    nb <- Poppler.documentGetNPages doc
    fmap (array (1,nb)) (traverse go [1..nb])
  where
    go i = do
        page  <- Poppler.documentGetPage doc (i-1)
        (w,h) <- Poppler.pageGetSize page
        return (i, PageItem page w h)

drawViewer :: Gtk.DrawingArea -> ViewerRef -> Gtk.EventM Gtk.EExpose ()
drawViewer area = liftIO . go
    where
      go ref = do
        ratio    <- viewerGetRatio ref
        page     <- viewerGetPageItem ref
        rects    <- viewerGetPageRects ref
        ove      <- viewerGetOvered ref
        sel      <- viewerGetSelected ref
        rectSel  <- viewerGetSelection ref
        evRect   <- fmap eventGetRect (viewerGetEvent ref)
        frame    <- Gtk.widgetGetDrawWindow area
        (fW, fH) <- Gtk.drawableGetSize frame
        let width  = ratio  * (pageWidth page)
            height = ratio  * (pageHeight page)
        viewerUpdateRulers ref
        Gtk.widgetSetSizeRequest area (truncate width) (truncate height)
        Gtk.renderWithDrawable frame $ do
                   Cairo.setSourceRGB 1.0 1.0 1.0
                   Cairo.rectangle 0 0 (fromIntegral fW) (fromIntegral fH)
                   Cairo.fill
                   Cairo.scale ratio ratio
                   Poppler.pageRender (pagePtr page)
                   drawRects 1.0 sel ove rects
                   drawingSel rectSel
                   drawRects 1.0 Nothing evRect evRect

      drawRects th sel ove = mapM_ (drawing th sel ove)

      drawing th sel ove r =
          let x = r ^. rectX
              y = r ^. rectY
              h = r ^. rectHeight
              w = r ^. rectWidth
              onSel s
                  | s == r    = Cairo.setSourceRGB 1.0 0 0
                  | otherwise = return ()
              onOver o
                  | o == r    = Cairo.setSourceRGB 0.16 0.72 0.92
                  | otherwise = return ()
              step _ = Cairo.setSourceRGB 0 0 1.0 in
          do Cairo.setSourceRGB 0 0 1.0
             mapM_ onOver ove
             mapM_ onSel sel
             Cairo.setLineWidth th
             Cairo.rectangle x y w h
             Cairo.closePath
             Cairo.stroke

      drawingSel = mapM_ go
          where
            go r =
                let x = r ^. rectX
                    y = r ^. rectY
                    h = r ^. rectHeight
                    w = r ^. rectWidth in
                do  Cairo.setSourceRGB 0 1.0 0
                    Cairo.setLineWidth 1
                    Cairo.rectangle x y w h
                    Cairo.closePath
                    Cairo.stroke
