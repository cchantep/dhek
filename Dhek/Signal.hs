--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Signal
--
--
--------------------------------------------------------------------------------
module Dhek.Signal where

--------------------------------------------------------------------------------
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Data.Foldable (for_, traverse_)
import Data.Functor (void)

--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Trans (liftIO)
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
import Dhek.Action (onNext, onPrev, onMinus, onPlus, onRem, onApplidok)
import Dhek.Draw
import Dhek.GUI
import Dhek.GUI.Action
import Dhek.File (onJsonImport, onJsonSave)
import Dhek.Free
import Dhek.I18N
import Dhek.Instr
import Dhek.Property (onProp)
import Dhek.Engine
import Dhek.Selection (onSel)
import Dhek.Types

--------------------------------------------------------------------------------
connectSignals :: GUI -> Interpreter -> IO ()
connectSignals g i = do
    Gtk.onDelete (guiWindow g) $ \_ -> do
        hasEvent <- engineHasEvents i
        case () of
            _ | hasEvent -> do
                resp <- gtkShowConfirm g (guiTranslate g $ MsgConfirm)
                return $ not resp
              | otherwise -> return False

    Gtk.on (guiPdfOpenMenuItem g) Gtk.menuItemActivate $ do
        resp <- Gtk.dialogRun $ guiPdfDialog g
        Gtk.widgetHide $ guiPdfDialog g
        case resp of
            Gtk.ResponseOk -> do
                uriOpt  <- Gtk.fileChooserGetURI $ guiPdfDialog g
                traverse_ (loadPdf i) uriOpt
            _ -> return ()

    Gtk.on (guiJsonOpenMenuItem g) Gtk.menuItemActivate $
        void $ runProgram i onJsonImport

    Gtk.on (guiJsonSaveMenuItem g) Gtk.menuItemActivate $
        void $ runProgram i onJsonSave

    Gtk.on (guiOverlapMenuItem g) Gtk.menuItemActivate $ void $ runProgram i $
        compile $ do
            b <- isActive Overlap
            active Overlap (not b)

    -- Previous Button ---
    Gtk.on (guiPrevButton g) Gtk.buttonActivated $ void $ runProgram i onPrev

    --- Next Button ---
    Gtk.on (guiNextButton g) Gtk.buttonActivated $ void $ runProgram i onNext

    --- Minus Button ---
    Gtk.on (guiZoomOutButton g) Gtk.buttonActivated $ void $ runProgram i onMinus

    --- Plus Button ---
    Gtk.on (guiZoomInButton g) Gtk.buttonActivated $ void $ runProgram i onPlus
    Gtk.on (guiRemoveButton g) Gtk.buttonActivated $ void $ runProgram i onRem
    _ <- Gtk.on (guiApplyButton g) Gtk.buttonActivated $
        void $ runProgram i (onProp g)

    --- Selection ---
    Gtk.on (guiRectTreeSelection g) Gtk.treeSelectionSelectionChanged $ do
        void $ runProgram i onSel

    Gtk.on (guiDrawingArea g) Gtk.exposeEvent $ Gtk.tryEvent $ liftIO $ do
        s    <- engineCurrentState i
        popt <- engineCurrentPage i
        opt  <- engineRatio i
        for_ ((,) <$> popt <*> opt) $ \(page, ratio)-> do
            engineRunDraw i
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

    Gtk.on (guiDrawingArea g) Gtk.motionNotifyEvent $ Gtk.tryEvent $ do
        pos@(x,y) <- Gtk.eventCoordinates
        liftIO $ do
            opt <- engineRatio i
            for_ opt $ \r -> do
                drawInterpret move i pos
                Gtk.set (guiHRuler g) [Gtk.rulerPosition Gtk.:= x/r]
                Gtk.set (guiVRuler g) [Gtk.rulerPosition Gtk.:= y/r]

    Gtk.on (guiDrawingArea g) Gtk.buttonPressEvent $ Gtk.tryEvent $ do
       pos <- Gtk.eventCoordinates
       b   <- Gtk.eventButton
       when (b == Gtk.LeftButton) $
           liftIO $ drawInterpret press i pos

    Gtk.on (guiDrawingArea g) Gtk.buttonReleaseEvent $ Gtk.tryEvent $ do
        pos <- Gtk.eventCoordinates
        liftIO $ drawInterpret (const release) i pos

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

    _ <- Gtk.on (guiTypeCombo g) Gtk.changed $ do
        opt <- Gtk.comboBoxGetActiveText $ guiTypeCombo g
        for_ opt $ \c -> do
            gtkSetValuePropVisible (c == "radio") g
            gtkSetIndexPropVisible (c == "textcell") g

    Gtk.on (guiVRuler g) Gtk.buttonPressEvent $ Gtk.tryEvent $ liftIO $
        void $ runProgram i $ compile $ newGuide GuideVertical

    Gtk.on (guiVRuler g) Gtk.motionNotifyEvent $ Gtk.tryEvent $ do
        (x',y') <- Gtk.eventCoordinates
        liftIO $ do
            opt <- engineRatio i
            v   <- Gtk.adjustmentGetValue (guiHRulerAdjustment g)

            for_ opt $ \ratio -> do
                let (x,y) = ((x'-25+v)/ratio, y'/ratio)

                Gtk.set (guiHRuler g) [Gtk.rulerPosition Gtk.:= x]
                Gtk.set (guiVRuler g) [Gtk.rulerPosition Gtk.:= y]

            void $ runProgram i $ compile $ do
                updateGuide
                draw

    Gtk.on (guiVRuler g) Gtk.buttonReleaseEvent $ Gtk.tryEvent $ liftIO $
        void $ runProgram i $ compile $ do
            addGuide
            draw

    Gtk.on (guiHRuler g) Gtk.buttonPressEvent $ Gtk.tryEvent $ liftIO $
        void $ runProgram i $ compile $ newGuide GuideHorizontal

    Gtk.on (guiHRuler g)  Gtk.motionNotifyEvent $ Gtk.tryEvent $ do
        (x',y') <- Gtk.eventCoordinates
        liftIO $ do
            opt <- engineRatio i
            v   <- Gtk.adjustmentGetValue (guiVRulerAdjustment g)

            for_ opt $ \ratio -> do
                let (x,y) = (x'/ratio, (y'+v-25)/ratio)

                Gtk.set (guiHRuler g) [Gtk.rulerPosition Gtk.:= x]
                Gtk.set (guiVRuler g) [Gtk.rulerPosition Gtk.:= y]

            void $ runProgram i $ compile $ do
                updateGuide
                draw

    Gtk.on (guiHRuler g) Gtk.buttonReleaseEvent $ Gtk.tryEvent $ liftIO $
        liftIO $ void $ runProgram i $ compile $ do
            addGuide
            draw

    Gtk.on (guiDrawToggle g) Gtk.buttonActivated $ liftIO $ do
        active <- Gtk.toggleButtonGetActive (guiDrawToggle g)
        when active $ do
            Gtk.toggleButtonSetActive (guiDupToggle g) False
            Gtk.toggleButtonSetActive (guiMultiSelToggle g) False
            engineSetMode DhekNormal i

    Gtk.on (guiDupToggle g) Gtk.toggled $ liftIO $ do
        active <- Gtk.toggleButtonGetActive (guiDupToggle g)
        when active $ do
            Gtk.toggleButtonSetActive (guiDrawToggle g) False
            Gtk.toggleButtonSetActive (guiMultiSelToggle g) False
            engineSetMode DhekDuplication i

    Gtk.on (guiMultiSelToggle g) Gtk.toggled $ liftIO $ do
        active <- Gtk.toggleButtonGetActive (guiMultiSelToggle g)
        when active $ do
            Gtk.toggleButtonSetActive (guiDupToggle g) False
            Gtk.toggleButtonSetActive (guiDrawToggle g) False
            engineSetMode DhekSelection i

    --- Applidok Button ---
    Gtk.on (guiDokButton g) Gtk.buttonActivated $ onApplidok

    return ()
