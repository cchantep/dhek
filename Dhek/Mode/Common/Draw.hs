--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Mode.Common.Draw
--
--------------------------------------------------------------------------------
module Dhek.Mode.Common.Draw where

--------------------------------------------------------------------------------
import           Control.Lens
import qualified Graphics.Rendering.Cairo as Cairo

--------------------------------------------------------------------------------
import Dhek.Types

--------------------------------------------------------------------------------
data RGB = RGB Double Double Double

--------------------------------------------------------------------------------
data Style = Line
           | Dash

--------------------------------------------------------------------------------
rgbRed :: RGB
rgbRed = RGB 1 0 0

--------------------------------------------------------------------------------
rgbGreen :: RGB
rgbGreen = RGB 0 1 0

--------------------------------------------------------------------------------
rgbBlue :: RGB
rgbBlue = RGB 0 0 1

--------------------------------------------------------------------------------
rgbWhite :: RGB
rgbWhite = RGB 1 1 1

--------------------------------------------------------------------------------
drawRectGuides :: Double -- Frame width
               -> Double -- Frame height
               -> RGB
               -> Rect
               -> Cairo.Render ()
drawRectGuides fw fh (RGB red green blue) r = do
    let rectLeft = r ^. rectX
        rectTop = r ^. rectY
        rectBot = rectTop + r ^. rectHeight
        rectRight = rectLeft + r ^. rectWidth

    -- Prepare draw
    --TODO: Cairo.setDash [1.0]
    Cairo.setLineWidth 0.5
    Cairo.setSourceRGB red green blue

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

--------------------------------------------------------------------------------
drawRect :: Double -- Frame width
         -> Double -- Frame Height
         -> RGB
         -> Style
         -> Rect
         -> Cairo.Render ()
drawRect fw fh (RGB red green blue) s r = do
    let x = r ^. rectX
        y = r ^. rectY
        h = r ^. rectHeight
        w = r ^. rectWidth

    Cairo.save
    Cairo.setSourceRGB red green blue
    case s of
        Line -> Cairo.setLineWidth 1
        Dash -> Cairo.setDash [5] 5
    Cairo.rectangle x y w h
    Cairo.closePath
    Cairo.stroke
    Cairo.restore

--------------------------------------------------------------------------------
drawGuide :: Double -- Width
          -> Double -- Height
          -> RGB
          -> Guide
          -> Cairo.Render ()
drawGuide w h (RGB red green blue) (Guide x typ)
    = do Cairo.setSourceRGB red green blue
         Cairo.setLineWidth 0.5
         case typ of
             GuideVertical   -> do
                 Cairo.moveTo x 0.0
                 Cairo.lineTo x h
             GuideHorizontal -> do
                 Cairo.moveTo 0.0 x
                 Cairo.lineTo w x
