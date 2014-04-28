--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Signal
--
--
--------------------------------------------------------------------------------
module Dhek.Signal where

--------------------------------------------------------------------------------
import Control.Monad (when)
import Data.Foldable (for_, traverse_)

--------------------------------------------------------------------------------
import           Control.Monad.Trans (liftIO)
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
import Dhek.Action (onNext, onPrev, onMinus, onPlus, onRem)
import Dhek.Draw (cairoDraw)
import Dhek.GUI
import Dhek.File (onJsonImport, onJsonSave)
import Dhek.Instr
import Dhek.Move (onMove, onPress, onRelease)
import Dhek.Property (onProp)
import Dhek.Engine
import Dhek.Selection (onSel)
import Dhek.Types

--------------------------------------------------------------------------------
connectSignals :: GUI -> Interpreter -> IO ()
connectSignals g i = do
    Gtk.on (guiPdfOpenMenuItem g) Gtk.menuItemActivate $ do
        resp <- Gtk.dialogRun $ guiPdfDialog g
        Gtk.widgetHide $ guiPdfDialog g
        case resp of
            Gtk.ResponseOk -> do
                uriOpt  <- Gtk.fileChooserGetURI $ guiPdfDialog g
                traverse_ (loadPdf i) uriOpt
            _ -> return ()
    Gtk.on (guiJsonOpenMenuItem g) Gtk.menuItemActivate $
        runProgram i onJsonImport
    Gtk.on (guiJsonSaveMenuItem g) Gtk.menuItemActivate $
        runProgram i onJsonSave
    Gtk.on (guiOverlapMenuItem g) Gtk.menuItemActivate $ runProgram i $
        compile $ do
            b <- isActive Overlap
            active Overlap (not b)
    -- Previous Button ---
    Gtk.on (guiPrevButton g) Gtk.buttonActivated $ runProgram i onPrev
    --- Next Button ---
    Gtk.on (guiNextButton g) Gtk.buttonActivated $ runProgram i onNext
    --- Minus Button ---
    Gtk.on (guiZoomOutButton g) Gtk.buttonActivated $ runProgram i onMinus
    --- Plus Button ---
    Gtk.on (guiZoomInButton g) Gtk.buttonActivated $ runProgram i onPlus
    Gtk.on (guiRemoveButton g) Gtk.buttonActivated $ runProgram i onRem
    Gtk.on (guiApplyButton g) Gtk.buttonActivated $ runProgram i onProp
    --- Selection ---
    Gtk.on (guiRectTreeSelection g) Gtk.treeSelectionSelectionChanged $ do
        runProgram i onSel
    Gtk.on (guiDrawingArea g) Gtk.exposeEvent $ Gtk.tryEvent $ liftIO $ do
        let action = compile $ do
                ratio <- getRatio
                performIO $ do
                    hsize  <- Gtk.adjustmentGetPageSize $ guiHRulerAdjustment g
                    vsize  <- Gtk.adjustmentGetPageSize $ guiVRulerAdjustment g
                    hlower <- Gtk.adjustmentGetLower $ guiHRulerAdjustment g
                    hupper <- Gtk.adjustmentGetUpper $ guiHRulerAdjustment g
                    hincr  <- Gtk.adjustmentGetPageIncrement $ guiHRulerAdjustment g
                    sincr  <- Gtk.adjustmentGetStepIncrement $ guiHRulerAdjustment g
                    vincr  <- Gtk.adjustmentGetPageIncrement $ guiVRulerAdjustment g
                    hvalue <- Gtk.adjustmentGetValue $ guiHRulerAdjustment g
                    vvalue <- Gtk.adjustmentGetValue $ guiVRulerAdjustment g
                    let w  = (hsize/ratio) + hl
                        h  = (vsize/ratio) + vl
                        hl = hvalue / ratio
                        vl = vvalue / ratio
                    Gtk.set (guiHRuler g) [ Gtk.rulerLower   Gtk.:= hl
                                          , Gtk.rulerUpper   Gtk.:= w
                                          , Gtk.rulerMaxSize Gtk.:= w
                                          ]
                    Gtk.set (guiVRuler g) [ Gtk.rulerLower   Gtk.:= vl
                                          , Gtk.rulerUpper   Gtk.:= h
                                          , Gtk.rulerMaxSize Gtk.:= h
                                          ]
        runProgram i (cairoDraw >> action)
    Gtk.on (guiDrawingArea g) Gtk.motionNotifyEvent $ Gtk.tryEvent $ do
        (x',y') <- Gtk.eventCoordinates
        let action = compile $ do
                (x,y) <- getPointer
                performIO $ do
                    Gtk.set (guiHRuler g) [Gtk.rulerPosition Gtk.:= x]
                    Gtk.set (guiVRuler g) [Gtk.rulerPosition Gtk.:= y]

        liftIO $ runProgramWithCoord i (x', y') (onMove >> action)
    Gtk.on (guiDrawingArea g) Gtk.buttonPressEvent $ Gtk.tryEvent $ do
       (x',y') <- Gtk.eventCoordinates
       b       <- Gtk.eventButton
       when (b == Gtk.LeftButton) $
           liftIO $ runProgramWithCoord i (x', y') onPress
    Gtk.on (guiDrawingArea g) Gtk.buttonReleaseEvent $ Gtk.tryEvent $ liftIO $
        runProgram i onRelease
    Gtk.on (guiDrawingArea g) Gtk.scrollEvent $ Gtk.tryEvent $ do
        dir <- Gtk.eventScrollDirection
        liftIO $ do
            pageSize <- Gtk.adjustmentGetPageSize $ guiVRulerAdjustment g
            lower    <- Gtk.adjustmentGetLower $ guiVRulerAdjustment g
            upper    <- Gtk.adjustmentGetUpper $ guiVRulerAdjustment g
            step     <- Gtk.adjustmentGetStepIncrement $ guiVRulerAdjustment g
            oldValue <- Gtk.adjustmentGetValue $ guiVRulerAdjustment g
            let delta' = step * 2
                delta  = case dir of
                    Gtk.ScrollUp -> negate delta'
                    _            -> delta'
                newValue = min (upper - pageSize) (max 0 (oldValue + delta))
            Gtk.adjustmentSetValue (guiVRulerAdjustment g) newValue
    Gtk.on (guiTypeCombo g) Gtk.changed $ do
        runProgram i $ compile $ do
            opt <- getComboText PropCombo
            for_ opt $ \c ->
                if c == "radio"
                then setValuePropVisible True
                else setValuePropVisible False
    Gtk.on (guiVRuler g) Gtk.buttonPressEvent $ Gtk.tryEvent $ liftIO $
        runProgram i $ compile $ newGuide GuideVertical
    Gtk.on (guiVRuler g) Gtk.motionNotifyEvent $ Gtk.tryEvent $ do
        (x',y') <- Gtk.eventCoordinates
        liftIO $ runProgramWithCoord i (x', y') $ compile $ do
            ratio <- getRatio
            performIO $ do
                v <- Gtk.adjustmentGetValue (guiHRulerAdjustment g)
                let (x,y) = ((x'-25+v)/ratio, y'/ratio)
                Gtk.set (guiHRuler g) [Gtk.rulerPosition Gtk.:= x]
                Gtk.set (guiVRuler g) [Gtk.rulerPosition Gtk.:= y]
            updateGuide
            draw
    Gtk.on (guiVRuler g) Gtk.buttonReleaseEvent $ Gtk.tryEvent $ liftIO $
        runProgram i $ compile $ do
            addGuide
            draw
    Gtk.on (guiHRuler g) Gtk.buttonPressEvent $ Gtk.tryEvent $ liftIO $
        runProgram i $ compile $ newGuide GuideHorizontal
    Gtk.on (guiHRuler g)  Gtk.motionNotifyEvent $ Gtk.tryEvent $ do
        (x',y') <- Gtk.eventCoordinates
        liftIO $ runProgramWithCoord i (x', y') $ compile $ do
            ratio <- getRatio
            performIO $ do
                v <- Gtk.adjustmentGetValue (guiVRulerAdjustment g)
                let (x,y) = (x'/ratio, (y'+v-25)/ratio)
                Gtk.set (guiHRuler g) [Gtk.rulerPosition Gtk.:= x]
                Gtk.set (guiVRuler g) [Gtk.rulerPosition Gtk.:= y]
            updateGuide
            draw
    Gtk.on (guiHRuler g) Gtk.buttonReleaseEvent $ Gtk.tryEvent $ liftIO $
        liftIO $ runProgram i $ compile $ do
            addGuide
            draw
    Gtk.on (guiDrawToggle g) Gtk.toggled $ liftIO $
        runProgram i $ compile $ do
            b <- isToggleActive DrawToggle
            setToggleActive MultiSelToggle (not b)
    Gtk.on (guiMultiSelToggle g) Gtk.toggled $ liftIO $
        runProgram i $ compile $ do
            b <- isToggleActive MultiSelToggle
            setToggleActive DrawToggle (not b)
    return ()
