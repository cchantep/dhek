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
import           System.FilePath (takeFileName)

--------------------------------------------------------------------------------
import Dhek.Action (onNext, onPrev, onMinus, onPlus, onRem, onApplidok)
import Dhek.Engine.Instr
import Dhek.GUI
import Dhek.GUI.Action
import Dhek.File (onJsonImport, onJsonSave)
import Dhek.I18N
import Dhek.PDF.Inlined
import Dhek.Property (onProp)
import Dhek.Engine
import Dhek.Selection (onSel)
import Dhek.Types
import Dhek.Widget.BlankDocument
import Dhek.Widget.Type

--------------------------------------------------------------------------------
connectSignals :: GUI -> RuntimeEnv -> IO ()
connectSignals g i = do
    Gtk.onDelete (guiWindow g) $ \_ ->
        do hasEvent <- engineHasEvents i
           case () of
               _ | hasEvent ->
                   do resp <- gtkShowConfirm g (guiTranslate g $ MsgConfirmQuit)
                      case resp of
                          DhekSave ->
                              do r <- runProgram i onJsonSave
                                 return $ not r
                          DhekDontSave -> return False
                          DhekCancel   -> return True
                 | otherwise -> return False

    Gtk.on (guiPdfOpenMenuItem g) Gtk.menuItemActivate $ dhekOpenPdf g i

    Gtk.on (guiSplashOpen g) Gtk.buttonActivated $ dhekOpenPdf g i

    Gtk.on (guiSplashDok g) Gtk.buttonActivated onApplidok

    Gtk.on (guiJsonOpenMenuItem g) Gtk.menuItemActivate $
        void $ runProgram i $ onJsonImport g

    Gtk.on (guiJsonSaveMenuItem g) Gtk.menuItemActivate $
        void $ runProgram i onJsonSave

    Gtk.on (guiOverlapMenuItem g) Gtk.menuItemActivate $ void $ runProgram i $
        do b <- isActive Overlap
           active Overlap (not b)

    -- Previous Button ---
    Gtk.onToolButtonClicked (guiPrevButton g) $
        do runProgram i onPrev
           guiClearPdfCache g

    --- Next Button ---
    Gtk.onToolButtonClicked (guiNextButton g) $
        do runProgram i onNext
           guiClearPdfCache g

    --- Minus Button ---
    Gtk.onToolButtonClicked (guiZoomOutButton g) $
        do runProgram i onMinus
           guiClearPdfCache g

    --- Plus Button ---
    Gtk.onToolButtonClicked (guiZoomInButton g) $
        do runProgram i onPlus
           guiClearPdfCache g

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
            engineModeDraw i
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
        mod       <- Gtk.eventModifier
        liftIO $
            do opt <- engineRatio i
               for_ opt $ \r ->
                   do engineModeMove mod i pos
                      Gtk.set (guiHRuler g) [Gtk.rulerPosition Gtk.:= x/r]
                      Gtk.set (guiVRuler g) [Gtk.rulerPosition Gtk.:= y/r]

    Gtk.on (guiDrawingArea g) Gtk.keyPressEvent $ Gtk.tryEvent $
        do name <- Gtk.eventKeyName
           mod  <- Gtk.eventModifier
           liftIO $ engineModeKeyPress mod name i

    Gtk.on (guiDrawingArea g) Gtk.keyReleaseEvent $ Gtk.tryEvent $
        do name <- Gtk.eventKeyName
           mod  <- Gtk.eventModifier
           liftIO $ engineModeKeyRelease mod name i

    Gtk.on (guiDrawingArea g) Gtk.enterNotifyEvent $ Gtk.tryEvent $ liftIO $
        do Gtk.widgetGrabFocus $ guiDrawingArea g
           engineModeEnter i

    Gtk.on (guiDrawingArea g) Gtk.leaveNotifyEvent $ Gtk.tryEvent $ liftIO $
        engineModeLeave i

    Gtk.on (guiDrawingArea g) Gtk.buttonPressEvent $ Gtk.tryEvent $ do
       pos <- Gtk.eventCoordinates
       b   <- Gtk.eventButton
       mod <- Gtk.eventModifier
       when (b == Gtk.LeftButton) $ liftIO $
           engineModePress mod i pos

    Gtk.on (guiDrawingArea g) Gtk.buttonReleaseEvent $ Gtk.tryEvent $ do
        pos <- Gtk.eventCoordinates
        mod <- Gtk.eventModifier
        liftIO $ engineModeRelease mod i pos

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
        void $ runProgram i $ guideNew GuideVertical

    Gtk.on (guiVRuler g) Gtk.motionNotifyEvent $ Gtk.tryEvent $ do
        (x',y') <- Gtk.eventCoordinates
        liftIO $ do
            opt <- engineRatio i
            v   <- Gtk.adjustmentGetValue (guiHRulerAdjustment g)

            for_ opt $ \ratio -> do
                let (x,y) = ((x'-25+v)/ratio, y'/ratio)

                Gtk.set (guiHRuler g) [Gtk.rulerPosition Gtk.:= x]
                Gtk.set (guiVRuler g) [Gtk.rulerPosition Gtk.:= y]

            void $ runProgram i $ do
                guideUpdate
                draw

    Gtk.on (guiVRuler g) Gtk.buttonReleaseEvent $ Gtk.tryEvent $ liftIO $
        void $ runProgram i $ do
            guideAdd
            draw

    Gtk.on (guiHRuler g) Gtk.buttonPressEvent $ Gtk.tryEvent $ liftIO $
        void $ runProgram i $ guideNew GuideHorizontal

    Gtk.on (guiHRuler g)  Gtk.motionNotifyEvent $ Gtk.tryEvent $ do
        (x',y') <- Gtk.eventCoordinates
        liftIO $ do
            opt <- engineRatio i
            v   <- Gtk.adjustmentGetValue (guiVRulerAdjustment g)

            for_ opt $ \ratio -> do
                let (x,y) = (x'/ratio, (y'+v-25)/ratio)

                Gtk.set (guiHRuler g) [Gtk.rulerPosition Gtk.:= x]
                Gtk.set (guiVRuler g) [Gtk.rulerPosition Gtk.:= y]

            void $ runProgram i $ do
                guideUpdate
                draw

    Gtk.on (guiHRuler g) Gtk.buttonReleaseEvent $ Gtk.tryEvent $ liftIO $
        liftIO $ void $ runProgram i $ do
            guideAdd
            draw

    Gtk.onToolButtonToggled (guiDrawToggle g) $ liftIO $ do
        active <- Gtk.toggleToolButtonGetActive (guiDrawToggle g)
        when active $ do
            Gtk.toggleToolButtonSetActive (guiDupToggle g) False
            Gtk.toggleToolButtonSetActive (guiMultiSelToggle g) False
            engineSetMode DhekNormal i

    Gtk.onToolButtonToggled (guiDupToggle g) $ liftIO $ do
        active <- Gtk.toggleToolButtonGetActive (guiDupToggle g)
        when active $ do
            Gtk.toggleToolButtonSetActive (guiDrawToggle g) False
            Gtk.toggleToolButtonSetActive (guiMultiSelToggle g) False
            engineSetMode DhekDuplication i

    Gtk.onToolButtonToggled (guiMultiSelToggle g) $ liftIO $ do
        active <- Gtk.toggleToolButtonGetActive (guiMultiSelToggle g)
        when active $ do
            Gtk.toggleToolButtonSetActive (guiDupToggle g) False
            Gtk.toggleToolButtonSetActive (guiDrawToggle g) False
            engineSetMode DhekSelection i

    --- Applidok Button ---
    _ <- Gtk.onToolButtonClicked (guiDokButton g) onApplidok

    -- Blank document item
    _ <- Gtk.on (guiOpenBlankMenuItem g) Gtk.menuItemActivate $
         widgetShow $ guiBlankDocumentWidget g

    -- Blank Document Open
    _ <- widgetRegister (guiBlankDocumentWidget g) BlankDocumentOpen $
         \(OpenInput dim pc) ->
             do doc <- loadPdfInlinedDoc (pdfTemplate dim pc)
                v   <- makeViewer (guiTranslate g MsgBlankDocument) doc
                loadViewer i v

    return ()

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
dhekOpenPdf :: GUI -> RuntimeEnv -> IO ()
dhekOpenPdf g i
    = do resp <- Gtk.dialogRun $ guiPdfDialog g
         Gtk.widgetHide $ guiPdfDialog g
         case resp of
             Gtk.ResponseOk ->
                 do uriOpt  <- Gtk.fileChooserGetURI $ guiPdfDialog g
                    for_ uriOpt $ \uri ->
                        do doc <- loadPdfFileDoc uri
                           v   <- makeViewer (takeFileName uri) doc
                           loadViewer i v
             _ -> return ()

--------------------------------------------------------------------------------
statusModPressed :: [Gtk.Modifier] -> Bool
statusModPressed xs
    | [Gtk.Alt] <- xs = True
    | otherwise       = False

--------------------------------------------------------------------------------
statusNamePressed :: String -> Bool
statusNamePressed n
    | "Alt_L" <- n = True
    | "Alt_R" <- n = True
    | otherwise    = False
