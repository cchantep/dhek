--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Draw
--
-- Gtk.Cairo drawing procedure
--------------------------------------------------------------------------------
module Dhek.Draw where

--------------------------------------------------------------------------------
import Prelude hiding (mapM_)
import Data.Foldable (mapM_)

--------------------------------------------------------------------------------
import           Control.Lens ((^.), use)
import           Control.Monad.RWS hiding (mapM_)
import qualified Graphics.UI.Gtk              as Gtk
import qualified Graphics.Rendering.Cairo     as Cairo
import qualified Graphics.UI.Gtk.Poppler.Page as Poppler

--------------------------------------------------------------------------------
import Dhek.Types

--------------------------------------------------------------------------------
data DrawParams
    = DrawParams
      { dpRatio     :: !Double
      , dpSelected  :: !(Maybe Rect)
      , dpSelection :: !(Maybe Rect)
      , dpOvered    :: !(Maybe Rect)
      , dpRects     :: ![Rect]
      , dpEvent     :: !(Maybe BoardEvent)
      , dpPage      :: !PageItem
      , dpGuides    :: ![Guide]
      , dpCurGuide  :: !(Maybe Guide)
      , dpArea      :: !Gtk.DrawingArea
      }

--------------------------------------------------------------------------------
cairoDraw :: DrawParams -> IO ()
cairoDraw p = do
    let ratio     = dpRatio p
        selected  = dpSelected p
        selection = dpSelection p
        event     = dpEvent p
        page      = dpPage p
        overed    = dpOvered p
        rects     = dpRects p
        guides    = dpGuides p
        curGuide  = dpCurGuide p
        area      = dpArea p

    frame     <- Gtk.widgetGetDrawWindow area
    (fw',fh') <- Gtk.drawableGetSize frame

    let width  = ratio * (pageWidth page)
        height = ratio * (pageHeight page)
        fw     = fromIntegral fw'
        fh     = fromIntegral fh'
        eventR = event >>= eventGetRect

    Gtk.widgetSetSizeRequest area (truncate width) (truncate height)
    Gtk.renderWithDrawable frame $ do
        -- Paint page background in white
        Cairo.setSourceRGB 1.0 1.0 1.0
        Cairo.rectangle 0 0 fw fh
        Cairo.fill

        Cairo.scale ratio ratio
        Poppler.pageRender (pagePtr page)
        mapM_ (drawGuide fw fh) guides
        mapM_ (drawGuide fw fh) curGuide
        Cairo.closePath
        Cairo.stroke
        drawRects 1.0 fw fh selected overed rects
        drawingSel fw fh selection
        drawRects 1.0 fw fh eventR eventR eventR
  where
    drawRects lw fw fh sel ove = mapM_ (drawing lw fw fh sel ove)

    drawGuide w h (Guide x typ) = do
        Cairo.setSourceRGB 0.16 0.26 0.87
        Cairo.setLineWidth 0.5
        case typ of
            GuideVertical   -> do
                Cairo.moveTo x 0.0
                Cairo.lineTo x h
            GuideHorizontal -> do
                Cairo.moveTo 0.0 x
                Cairo.lineTo w x

    -- lw: Line width
    -- fw: Frame width
    -- fh: Frame height
    -- sel: selection
    drawing lw fw fh sel ove r =
        let x = r ^. rectX
            y = r ^. rectY
            h = r ^. rectHeight
            w = r ^. rectWidth
            onSel s
                | s == r    = selRect r
                | otherwise = return ()
            onOver o
                | o == r    = Cairo.setSourceRGB 0.16 0.72 0.92
                | otherwise = return ()
            step _ = Cairo.setSourceRGB 0 0 1.0 in
        do Cairo.setSourceRGB 0 0 1.0
           mapM_ onOver ove
           mapM_ onSel sel
           Cairo.setLineWidth lw
           Cairo.rectangle x y w h
           Cairo.closePath
           Cairo.stroke
        where
          selRect r = do
                 drawRectGuides fw fh r

                 -- Then prepare line color for rect
                 Cairo.setSourceRGB 1.0 0 0

    -- Draws temporary guides around selected rectangle r
    -- fw: Frame width
    -- fh: Frame height
    -- r: Rectangle
    drawRectGuides :: Double -> Double -> Rect -> Cairo.Render()
    drawRectGuides fw fh r =
        let rectLeft = r ^. rectX
            rectTop = r ^. rectY
            rectBot = rectTop + r ^. rectHeight
            rectRight = rectLeft + r ^. rectWidth in
        do -- Prepare draw
           --TODO: Cairo.setDash [1.0]
           Cairo.setLineWidth 0.5
           Cairo.setSourceRGB 0.16 0.72 0.92

           -- Draw horizontal top line
           Cairo.moveTo 0 rectTop -- frameLeft,rectTop
           Cairo.lineTo fw rectTop -- line to frameRight,rectTop

           -- Draw horizontal bottom line
           Cairo.moveTo 0 rectBot -- frameLeft,rectBot
           Cairo.lineTo fw rectBot -- line to frameRight,rectBot

           -- Draw vertical left line
           Cairo.moveTo rectLeft 0 -- rectLeft,frameTop
           Cairo.lineTo rectLeft fh -- line to rectLeft,frameBottom

           -- Draw vertical right line
           Cairo.moveTo rectRight 0 -- rectRight,frameTop
           Cairo.lineTo rectRight fh -- line to rectRight,frameBottom

           Cairo.stroke

    drawingSel fw fh rs = mapM_ (go) rs
      where
        go r =
            let x = r ^. rectX
                y = r ^. rectY
                h = r ^. rectHeight
                w = r ^. rectWidth in
            do Cairo.setSourceRGB 0 1.0 0
               Cairo.setLineWidth 1
               Cairo.rectangle x y w h
               Cairo.closePath
               Cairo.stroke
               drawRectGuides fw fh r

--------------------------------------------------------------------------------
