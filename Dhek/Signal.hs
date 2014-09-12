--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Signal
--
--
--------------------------------------------------------------------------------
module Dhek.Signal where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad (when)
import Data.Foldable (for_, traverse_)
import Data.Functor (void)

--------------------------------------------------------------------------------
import           Control.Monad.Trans (liftIO)
import qualified Graphics.UI.Gtk as Gtk
import           System.FilePath (takeFileName)

--------------------------------------------------------------------------------
import Dhek.AppUtil (closeKeystrokes)
import Dhek.Action (onNext, onPrev, onMinus, onPlus, onRem, onApplidok)
import Dhek.Engine.Instr
import Dhek.GUI
import Dhek.GUI.Action
import Dhek.File (onJsonImport, onJsonSave)
import Dhek.I18N
import Dhek.Mode.Common.Draw
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
    _ <- Gtk.on (guiWindow g) Gtk.deleteEvent $ liftIO $ closeConfirmation i g

    _ <- Gtk.on (guiPdfOpenMenuItem g) Gtk.menuItemActivate $ dhekOpenPdf g i

    _ <- Gtk.on (guiSplashOpen g) Gtk.buttonActivated $ dhekOpenPdf g i

    _ <- Gtk.on (guiSplashDok g) Gtk.buttonActivated onApplidok

    _ <- Gtk.on (guiJsonOpenMenuItem g) Gtk.menuItemActivate $
         void $ runProgram i $ onJsonImport g

    _ <- Gtk.on (guiJsonSaveMenuItem g) Gtk.menuItemActivate $ runProgram i $
             do rs <- getAllRects
                let rs' = rs >>= snd
                if null rs'
                    then showWarning $ guiTranslate g MsgNoArea
                    else void onJsonSave

    _ <- Gtk.on (guiOverlapMenuItem g) Gtk.menuItemActivate $
             runProgram i $
                 do b <- isActive Overlap
                    active Overlap (not b)

    _ <- Gtk.on (guiMagneticForceMenuItem g) Gtk.menuItemActivate $
         runProgram i $
             do b <- isActive Magnetic
                active Magnetic (not b)

    -- Previous Button ---
    _ <- Gtk.onToolButtonClicked (guiPrevButton g) $
             do runProgram i onPrev
                guiClearPdfCache g

    --- Next Button ---
    _ <- Gtk.onToolButtonClicked (guiNextButton g) $
             do runProgram i onNext
                guiClearPdfCache g

    --- Minus Button ---
    _ <- Gtk.onToolButtonClicked (guiZoomOutButton g) $
             do runProgram i onMinus
                guiClearPdfCache g

    --- Plus Button ---
    _ <- Gtk.onToolButtonClicked (guiZoomInButton g) $
             do runProgram i onPlus
                guiClearPdfCache g

    _ <- Gtk.on (guiRemoveButton g) Gtk.buttonActivated $ void $
             runProgram i onRem
    _ <- Gtk.on (guiApplyButton g) Gtk.buttonActivated $
             runProgram i (onProp g)

    --- Selection ---
    _ <- Gtk.on (guiRectTreeSelection g) Gtk.treeSelectionSelectionChanged $
             void $ runProgram i onSel

    _ <- Gtk.on (guiDrawingArea g) Gtk.exposeEvent $ Gtk.tryEvent $ liftIO $
             do mRatio <- engineRatio i
                mPage  <- engineCurrentPage i
                for_ ((,) <$> mPage <*> mRatio) $ \(page, ratio) ->
                    do engineModeDraw i
                       hsize  <- Gtk.adjustmentGetPageSize $
                                 guiHRulerAdjustment g
                       vsize  <- Gtk.adjustmentGetPageSize $
                                 guiVRulerAdjustment g
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
                       runProgram i $
                           do gs <- getGuides
                              mG <- guideGetCurrent
                              let guideColor = RGB 196 160 0
                              liftIO $
                                  guiRenderGuides g ratio page guideColor gs
                              liftIO $
                                  traverse_
                                  (guiRenderGuide g ratio page rgbGreen)
                                  mG

    _ <- Gtk.on (guiDrawingArea g) Gtk.motionNotifyEvent $ Gtk.tryEvent $
             do pos@(x,y) <- Gtk.eventCoordinates
                modf      <- Gtk.eventModifier
                liftIO $
                    do opt <- engineRatio i
                       for_ opt $ \r ->
                           do engineModeMove modf i pos
                              Gtk.set (guiHRuler g)
                                  [Gtk.rulerPosition Gtk.:= x/r]
                              Gtk.set (guiVRuler g)
                                  [Gtk.rulerPosition Gtk.:= y/r]

    _ <- Gtk.after (guiWindow g) Gtk.keyPressEvent $ Gtk.tryEvent $
             do name <- Gtk.eventKeyName
                modf <- Gtk.eventModifier
                if closeKeystrokes name modf
                    then liftIO $
                         do stay <- closeConfirmation i g
                            when (not stay) $
                                Gtk.widgetDestroy $ guiWindow g
                    else liftIO $ engineModeKeyPress modf name i

    _ <- Gtk.after (guiWindow g) Gtk.keyReleaseEvent $ Gtk.tryEvent $
             do name <- Gtk.eventKeyName
                modf <- Gtk.eventModifier
                liftIO $ engineModeKeyRelease modf name i

    _ <- Gtk.on (guiDrawingArea g) Gtk.enterNotifyEvent $ Gtk.tryEvent $
             liftIO $ engineModeEnter i

    _ <- Gtk.on (guiDrawingArea g) Gtk.leaveNotifyEvent $ Gtk.tryEvent $
             liftIO $ engineModeLeave i

    _ <- Gtk.on (guiDrawingArea g) Gtk.buttonPressEvent $ Gtk.tryEvent $
             do pos  <- Gtk.eventCoordinates
                b    <- Gtk.eventButton
                modf <- Gtk.eventModifier
                when (b == Gtk.LeftButton) $ liftIO $
                    engineModePress modf i pos

    _ <- Gtk.on (guiDrawingArea g) Gtk.buttonReleaseEvent $ Gtk.tryEvent $
             do pos <- Gtk.eventCoordinates
                modf <- Gtk.eventModifier
                liftIO $ engineModeRelease modf i pos

    _ <- Gtk.on (guiDrawingArea g) Gtk.scrollEvent $ Gtk.tryEvent $
             do dir <- Gtk.eventScrollDirection
                liftIO $
                    do pageSize <- Gtk.adjustmentGetPageSize $
                                   guiVRulerAdjustment g
                       upper    <- Gtk.adjustmentGetUpper $
                                   guiVRulerAdjustment g
                       step     <- Gtk.adjustmentGetStepIncrement $
                                   guiVRulerAdjustment g
                       oldValue <- Gtk.adjustmentGetValue $
                                   guiVRulerAdjustment g
                       let delta' = step * 2
                           delta  = case dir of
                                        Gtk.ScrollUp -> negate delta'
                                        _            -> delta'
                           newValue = min (upper - pageSize)
                                      (max 0 (oldValue + delta))
                       Gtk.adjustmentSetValue (guiVRulerAdjustment g) newValue

    _ <- Gtk.on (guiTypeCombo g) Gtk.changed $
             do opt <- Gtk.comboBoxGetActiveText $ guiTypeCombo g
                for_ opt $ \c -> do
                    gtkSetValuePropVisible (c == "radio" || c == "comboitem") g
                    gtkSetIndexPropVisible (c == "textcell") g

    _ <- Gtk.on (guiVRuler g) Gtk.buttonPressEvent $ Gtk.tryEvent $ liftIO $
         void $ runProgram i $ guideNew GuideVertical

    _ <- Gtk.on (guiVRuler g) Gtk.motionNotifyEvent $ Gtk.tryEvent $
             do (x',y') <- Gtk.eventCoordinates
                liftIO $
                    do opt <- engineRatio i
                       v   <- Gtk.adjustmentGetValue $ guiHRulerAdjustment g
                       for_ opt $ \ratio ->
                           do let (x,y) = ((x'-25+v)/ratio, y'/ratio)

                              Gtk.set (guiHRuler g) [Gtk.rulerPosition Gtk.:= x]
                              Gtk.set (guiVRuler g) [Gtk.rulerPosition Gtk.:= y]
                       runProgram i $
                           do guideUpdate
                              draw

    _ <- Gtk.on (guiVRuler g) Gtk.buttonReleaseEvent $ Gtk.tryEvent $ liftIO $
             runProgram i $
                 do guideAdd
                    draw

    _ <- Gtk.on (guiHRuler g) Gtk.buttonPressEvent $ Gtk.tryEvent $ liftIO $
         runProgram i $ guideNew GuideHorizontal

    _ <- Gtk.on (guiHRuler g)  Gtk.motionNotifyEvent $ Gtk.tryEvent $
             do (x',y') <- Gtk.eventCoordinates
                liftIO $
                    do opt <- engineRatio i
                       v   <- Gtk.adjustmentGetValue $ guiVRulerAdjustment g
                       for_ opt $ \ratio ->
                           do let (x,y) = (x'/ratio, (y'+v-25)/ratio)
                              Gtk.set (guiHRuler g)
                                  [Gtk.rulerPosition Gtk.:= x]
                              Gtk.set (guiVRuler g)
                                  [Gtk.rulerPosition Gtk.:= y]

                       runProgram i $
                           do guideUpdate
                              draw

    _ <- Gtk.on (guiHRuler g) Gtk.buttonReleaseEvent $ Gtk.tryEvent $
             liftIO $ runProgram i $
                 do guideAdd
                    draw

    _ <- Gtk.onToolButtonToggled (guiDrawToggle g) $ liftIO $
             do drawActive <- Gtk.toggleToolButtonGetActive $ guiDrawToggle g
                when drawActive $
                    do Gtk.toggleToolButtonSetActive (guiDupToggle g) False
                       Gtk.toggleToolButtonSetActive (guiMultiSelToggle g) False
                       engineSetMode DhekNormal i

    _ <- Gtk.onToolButtonToggled (guiDupToggle g) $ liftIO $
             do dupActive <- Gtk.toggleToolButtonGetActive (guiDupToggle g)
                when dupActive $
                    do Gtk.toggleToolButtonSetActive (guiDrawToggle g) False
                       Gtk.toggleToolButtonSetActive (guiMultiSelToggle g) False
                       engineSetMode DhekDuplication i

    _ <- Gtk.onToolButtonToggled (guiMultiSelToggle g) $ liftIO $
             do selActive <- Gtk.toggleToolButtonGetActive (guiMultiSelToggle g)
                when selActive $
                    do Gtk.toggleToolButtonSetActive (guiDupToggle g) False
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

--------------------------------------------------------------------------------
closeConfirmation :: RuntimeEnv -> GUI -> IO Bool
closeConfirmation i g
    = do hasEvent <- engineHasEvents i
         case () of
             _ | hasEvent ->
                 do resp <- gtkShowConfirm g (guiTranslate g $ MsgConfirmQuit)
                    case resp of
                        DhekSave
                            -> do r <- runProgram i onJsonSave
                                  return $ not r
                        DhekDontSave -> return False
                        DhekCancel   -> return True
               | otherwise -> return False
