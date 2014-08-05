--------------------------------------------------------------------------------
-- |
-- Module : Dhek.GUI
--
-- This module declares everything related to the GUI like widgets
--
--------------------------------------------------------------------------------
module Dhek.GUI where

--------------------------------------------------------------------------------
import Prelude hiding (foldr)
import Control.Monad ((>=>), forM)
import Control.Monad.Trans (MonadIO(..))
import Data.Foldable (traverse_, foldr)
import Data.IORef
import Data.Maybe
import Foreign.Ptr

--------------------------------------------------------------------------------
import           Control.Lens ((^.))
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk          as Gtk
import qualified Graphics.UI.Gtk.Poppler.Page as Poppler
import           System.FilePath (joinPath, dropFileName)
import           System.Environment.Executable (getExecutablePath)

--------------------------------------------------------------------------------
import           Dhek.AppUtil (appTerminate)
import           Dhek.I18N
import qualified Dhek.Resources as Resources
import           Dhek.Types
import           Dhek.Widget.Type
import           Dhek.Widget.BlankDocument

--------------------------------------------------------------------------------
data GUI =
    GUI
    { guiWindow :: Gtk.Window
    , guiPdfDialog :: Gtk.FileChooserDialog
    , guiJsonOpenDialog :: Gtk.FileChooserDialog
    , guiJsonSaveDialog :: Gtk.FileChooserDialog
    , guiPdfOpenMenuItem :: Gtk.MenuItem
    , guiOpenBlankMenuItem :: Gtk.MenuItem
    , guiJsonOpenMenuItem :: Gtk.MenuItem
    , guiJsonSaveMenuItem :: Gtk.MenuItem
    , guiOverlapMenuItem :: Gtk.CheckMenuItem
    , guiPrevButton :: Gtk.ToolButton
    , guiNextButton :: Gtk.ToolButton
    , guiZoomInButton :: Gtk.ToolButton
    , guiZoomOutButton :: Gtk.ToolButton
    , guiRemoveButton :: Gtk.Button
    , guiApplyButton :: Gtk.Button
    , guiDrawToggle :: Gtk.ToggleToolButton
    , guiDupToggle :: Gtk.ToggleToolButton
    , guiMultiSelToggle :: Gtk.ToggleToolButton
    , guiVRuler :: Gtk.VRuler
    , guiHRuler :: Gtk.HRuler
    , guiDrawingArea :: Gtk.DrawingArea
    , guiRectStore :: Gtk.ListStore Rect
    , guiNameEntry :: Gtk.Entry
    , guiValueEntry :: Gtk.Entry
    , guiTypeCombo :: Gtk.ComboBox
    , guiRectTreeSelection :: Gtk.TreeSelection
    , guiTypeStore :: Gtk.ListStore String
    , guiValueEntryAlign :: Gtk.Alignment
    , guiWindowVBox :: Gtk.VBox
    , guiWindowHBox :: Gtk.HBox
    , guiVRulerAdjustment :: Gtk.Adjustment
    , guiHRulerAdjustment :: Gtk.Adjustment
    , guiModeToolbar :: Gtk.Toolbar
    , guiTranslate :: DhekMessage -> String
    , guiDokButton :: Gtk.ToolButton
    , guiIndexAlign :: Gtk.Alignment
    , guiIndexSpin :: Gtk.SpinButton
    , guiSplashAlign :: Gtk.Alignment
    , guiSplashOpen :: Gtk.Button
    , guiSplashDok :: Gtk.Button
    , guiPdfCache :: IORef (Maybe Cairo.Surface)
    , guiStatusBar :: Gtk.Statusbar
    , guiContextId :: Gtk.ContextId
    , guiDrawPopup :: Gtk.Window
    , guiBlankDocumentWidget :: Widget BlankDocumentEvent
    }

--------------------------------------------------------------------------------
initGUI :: IO [String]
initGUI = do
    gtk      <- Gtk.initGUI
    Gtk.settingsGetDefault >>=
        foldr (\s err -> settings gtk s) (fail "No GTK default settings")
  where
    settings :: [String] -> Gtk.Settings -> IO [String]
    settings gui gs = do
        Gtk.settingsSetLongProperty gs "gtk-button-images" 1 "Dhek"
        return gui

makeGUI :: IO GUI
makeGUI = do
    gtkUI <- initGUI

    -- Window creation
    win   <- Gtk.windowNew
    wvbox <- Gtk.vBoxNew False 10
    vbox  <- Gtk.vBoxNew False 10
    hbox  <- Gtk.hBoxNew False 10
    vleft <- Gtk.vBoxNew False 10
    Gtk.containerAdd win wvbox

    msgStr <- mkI18N

    -- PDF Dialog
    pdfch <- createDialog win
             (msgStr MsgOpenPDF)
             "*.pdf"
             (msgStr MsgPdfFilter)
             (msgStr MsgOpen)
             Gtk.FileChooserActionOpen
             msgStr

    -- JSON Save Dialog
    jsonSch <- createDialog win
              (msgStr MsgSaveMappings)
              "*.json"
              (msgStr MsgJsonFilter)
              (msgStr MsgSave)
              Gtk.FileChooserActionSave
              msgStr

    -- JSON Load Dialog
    jsonLch <- createDialog win
              (msgStr MsgLoadMappings)
              "*.json"
              (msgStr MsgJsonFilter)
              (msgStr MsgOpen)
              Gtk.FileChooserActionOpen
              msgStr

    -- Runtime directories
    execPath <- getExecutablePath
    let resDir = joinPath [dropFileName execPath, "resources"]

    -- Menu Bar
    mbar   <- Gtk.menuBarNew
    fmenu  <- Gtk.menuNew
    malign <- Gtk.alignmentNew 0 0 1 0
    fitem  <- Gtk.menuItemNewWithLabel $ msgStr MsgFile
    oitem  <- Gtk.menuItemNewWithLabel $ msgStr MsgOpenPDF
    ovitem <- Gtk.menuItemNewWithLabel $ msgStr MsgOpenBlank
    iitem  <- Gtk.menuItemNewWithLabel $ msgStr MsgLoadMappings
    sitem  <- Gtk.menuItemNewWithLabel $ msgStr MsgSaveMappings
    citem  <- Gtk.checkMenuItemNewWithLabel $ msgStr MsgEnableOverlap
    Gtk.menuShellAppend fmenu oitem
    Gtk.menuShellAppend fmenu ovitem
    Gtk.menuShellAppend fmenu iitem
    Gtk.menuShellAppend fmenu sitem
    Gtk.menuShellAppend fmenu citem
    Gtk.menuItemSetSubmenu fitem fmenu
    Gtk.menuShellAppend mbar fitem
    Gtk.containerAdd malign mbar
    Gtk.widgetSetSensitive iitem False
    Gtk.widgetSetSensitive sitem False
    Gtk.widgetSetSensitive citem False
    Gtk.boxPackStart wvbox malign Gtk.PackNatural 0

    -- Button Next
    nimg <- loadImage Resources.goNext
    next <- Gtk.toolButtonNew (Just nimg) Nothing

     -- Previous Prev
    pimg <- loadImage Resources.goPrevious
    prev <- Gtk.toolButtonNew (Just pimg) Nothing

    -- Button Zoom out
    oimg  <- loadImage Resources.zoomOut
    minus <- Gtk.toolButtonNew (Just oimg) Nothing

    -- Button Zoom in
    iimg <- loadImage Resources.zoomIn
    plus <- Gtk.toolButtonNew (Just iimg) Nothing

    -- Button Draw
    drwb <- Gtk.toggleToolButtonNew
    dimg <- loadImage Resources.drawRectangle
    Gtk.set drwb [Gtk.widgetTooltipText Gtk.:=
                  Just $ msgStr MsgNormalModeTooltip]
    Gtk.toolButtonSetIconWidget drwb $ Just dimg
    Gtk.toggleToolButtonSetActive drwb True

    -- Button Duplicate
    db   <- Gtk.toggleToolButtonNew
    dimg <- loadImage Resources.duplicateRectangle
    Gtk.toolButtonSetIconWidget db $ Just dimg

    -- Button MultiSelection
    msb  <- Gtk.toggleToolButtonNew
    simg <- loadImage Resources.rectangularSelection
    Gtk.toolButtonSetIconWidget msb $ Just simg

    -- Main Toolbar
    toolbar <- Gtk.toolbarNew
    Gtk.toolbarSetStyle toolbar Gtk.ToolbarIcons
    Gtk.toolbarSetIconSize toolbar (Gtk.IconSizeUser 32)
    vsep1   <- Gtk.separatorToolItemNew
    vsep2   <- Gtk.separatorToolItemNew
    Gtk.toolbarInsert toolbar prev (-1)
    Gtk.toolbarInsert toolbar next (-1)
    Gtk.toolbarInsert toolbar vsep1 (-1)
    Gtk.toolbarInsert toolbar minus (-1)
    Gtk.toolbarInsert toolbar plus (-1)
    Gtk.toolbarInsert toolbar vsep2 (-1)
    Gtk.toolbarInsert toolbar drwb (-1)
    Gtk.toolbarInsert toolbar db (-1)
    Gtk.toolbarInsert toolbar msb (-1)
    Gtk.boxPackStart vbox toolbar Gtk.PackNatural 0

    -- Button Applidok
    kimg <- loadImage Resources.applidok
    akb  <- Gtk.toolButtonNew (Just kimg) Nothing


    -- Mode toolbar
    mtoolbar <- Gtk.toolbarNew
    Gtk.toolbarSetStyle mtoolbar Gtk.ToolbarIcons
    Gtk.toolbarSetIconSize mtoolbar (Gtk.IconSizeUser 32)
    --mtalign  <- Gtk.alignmentNew 0 0 0 0
    -- Gtk.boxSetSpacing mtoolbar 2
    -- Gtk.buttonBoxSetLayout mtoolbar Gtk.ButtonboxEdge
    -- Gtk.containerAdd mtalign mtoolbar
    -- Gtk.containerAdd mtoolbar akb
    Gtk.toolbarInsert mtoolbar akb 0
    Gtk.boxPackStart vbox mtoolbar Gtk.PackNatural 0
    --Gtk.widgetSetSizeRequest mtoolbar (-1) 32

    -- Splash screen
    splash   <- Gtk.vBoxNew False 40
    splalign <- Gtk.alignmentNew 0.5 0.4 0 0
    splelign <- Gtk.alignmentNew 0 0 0 0
    splslign <- Gtk.alignmentNew 0 0 0 0
    splopen  <- Gtk.buttonNewWithLabel $ msgStr MsgSplashOpenPDFFile
    spledit  <- Gtk.labelNew $ Just $ msgStr MsgSplashEdit
    splsave  <- Gtk.labelNew $ Just $ msgStr MsgSplashSave
    spldok   <- Gtk.buttonNewWithLabel $ msgStr MsgSplashCopy
    Gtk.containerAdd splalign splash
    Gtk.containerAdd splelign spledit
    Gtk.containerAdd splslign splsave
    Gtk.buttonSetAlignment splopen (0, 0)
    Gtk.buttonSetAlignment spldok (0, 0)
    Gtk.boxPackStart splash splopen Gtk.PackRepel 0
    Gtk.boxPackStart splash splelign Gtk.PackNatural 0
    Gtk.boxPackStart splash splslign Gtk.PackNatural 0
    Gtk.boxPackStart splash spldok Gtk.PackNatural 0
    Gtk.containerAdd wvbox splalign

    -- Drawing Area tooltip
    drawpop <- Gtk.windowNewPopup
    dplabel <- Gtk.labelNew Nothing
    Gtk.labelSetMarkup dplabel $ msgStr MsgDuplicationModePopup
    Gtk.containerAdd drawpop dplabel
    Gtk.windowSetTypeHint drawpop Gtk.WindowTypeHintTooltip
    Gtk.widgetModifyBg drawpop Gtk.StateNormal (Gtk.Color 0 0 0)
    Gtk.widgetModifyFg dplabel Gtk.StateNormal (Gtk.Color 65000 65000 65000)

    -- Drawing Area
    area     <- Gtk.drawingAreaNew
    vruler   <- Gtk.vRulerNew
    hruler   <- Gtk.hRulerNew
    halign   <- Gtk.alignmentNew 0 0 1 1
    valign   <- Gtk.alignmentNew 0 0 0 1
    hadj     <- Gtk.adjustmentNew 0 0 0 0 0 0
    vadj     <- Gtk.adjustmentNew 0 0 0 0 0 0
    viewport <- Gtk.viewportNew hadj vadj
    hscroll  <- Gtk.hScrollbarNew hadj
    vscroll  <- Gtk.vScrollbarNew vadj
    tswin    <- Gtk.scrolledWindowNew Nothing Nothing
    atable   <- Gtk.tableNew 3 3 False
    atswin   <- Gtk.alignmentNew 0 0 1 1
    Gtk.containerAdd viewport area
    Gtk.set vruler [Gtk.rulerMetric Gtk.:= Gtk.Pixels]
    Gtk.set hruler [Gtk.rulerMetric Gtk.:= Gtk.Pixels]
    Gtk.widgetAddEvents area [ Gtk.PointerMotionMask
                             , Gtk.KeyPressMask
                             , Gtk.KeyReleaseMask
                             ]
    Gtk.widgetSetCanFocus area True
    Gtk.widgetSetSizeRequest viewport 200 200
    Gtk.widgetSetSizeRequest hruler 25 25
    Gtk.widgetSetSizeRequest vruler 25 25
    Gtk.tableSetRowSpacing atable 0 0
    Gtk.tableSetColSpacing atable 0 0
    let gtkTabAll  = [Gtk.Expand, Gtk.Shrink, Gtk.Fill]
        gtkTabView = [Gtk.Expand, Gtk.Fill]
    Gtk.tableAttach atable hruler 1 2 0 1 gtkTabAll [Gtk.Fill] 0 0
    Gtk.tableAttach atable hscroll 1 2 2 3 gtkTabAll [Gtk.Fill] 0 0
    Gtk.tableAttach atable vruler 0 1 1 2 [Gtk.Fill] gtkTabAll 0 0
    Gtk.tableAttach atable vscroll 2 3 1 2 [Gtk.Fill] gtkTabAll 0 0
    Gtk.tableAttach atable viewport 1 2 1 2 gtkTabView gtkTabView 0 0
    Gtk.boxPackStart vbox atable Gtk.PackGrow 0

    -- Area list
    store  <- Gtk.listStoreNew ([] :: [Rect])
    treeV  <- Gtk.treeViewNewWithModel store
    sel    <- Gtk.treeViewGetSelection treeV
    tswin  <- Gtk.scrolledWindowNew Nothing Nothing
    atswin <- Gtk.alignmentNew 0 0 1 1
    col    <- Gtk.treeViewColumnNew
    trend  <- Gtk.cellRendererTextNew

    Gtk.treeViewColumnSetTitle col $ msgStr $ MsgAreas
    Gtk.cellLayoutPackStart col trend False
    Gtk.cellLayoutSetAttributes col trend store layoutMapping
    Gtk.treeViewAppendColumn treeV col
    Gtk.scrolledWindowAddWithViewport tswin treeV
    Gtk.scrolledWindowSetPolicy tswin Gtk.PolicyAutomatic Gtk.PolicyAutomatic
    Gtk.containerAdd atswin tswin
    Gtk.boxPackStart vleft atswin Gtk.PackGrow 0
    Gtk.boxPackStart hbox vbox Gtk.PackGrow 0
    Gtk.boxPackStart hbox vleft Gtk.PackNatural 0

    -- Properties
    rem     <- Gtk.buttonNewWithLabel $ msgStr MsgRemove
    rmimg   <- loadImage Resources.drawEraser
    Gtk.buttonSetImage rem rmimg

    app     <- Gtk.buttonNewWithLabel $ msgStr MsgApply
    apimg   <- loadImage Resources.dialogAccept
    Gtk.buttonSetImage app apimg

    idxspin <- Gtk.spinButtonNewWithRange 0 200 1
    nlabel  <- Gtk.labelNew (Just $ msgStr MsgName)
    tlabel  <- Gtk.labelNew (Just $ msgStr MsgType)
    vlabel  <- Gtk.labelNew (Just $ msgStr MsgValue)
    idxlabel <- Gtk.labelNew (Just $ msgStr MsgIndex)
    pentry  <- Gtk.entryNew
    ventry  <- Gtk.entryNew
    ualign  <- Gtk.alignmentNew 0.5 0 0 0
    nalign  <- Gtk.alignmentNew 0 0.5 0 0
    talign  <- Gtk.alignmentNew 0 0.5 0 0
    valign  <- Gtk.alignmentNew 0 0.5 0 0
    idxalign <- Gtk.alignmentNew 0 0.5 0 0
    salign  <- Gtk.alignmentNew 0 0 1 0
    table   <- Gtk.tableNew 2 4 False
    tvbox   <- Gtk.vBoxNew False 10
    optvbox <- Gtk.vBoxNew False 10
    pcombo  <- Gtk.comboBoxNew
    tstore  <- Gtk.comboBoxSetModelText pcombo
    hsep    <- Gtk.hSeparatorNew
    arem    <- Gtk.alignmentNew 0.5 0 0 0
    aapp    <- Gtk.alignmentNew 0.5 0 0 0
    Gtk.containerAdd arem rem
    Gtk.containerAdd aapp app
    Gtk.containerAdd nalign nlabel
    Gtk.containerAdd talign tlabel
    Gtk.containerAdd valign vlabel
    Gtk.containerAdd idxalign idxlabel
    Gtk.tableAttachDefaults table nalign 0 1 0 1
    Gtk.tableAttachDefaults table pentry 1 2 0 1
    Gtk.tableAttachDefaults table talign 0 1 1 2
    Gtk.tableAttachDefaults table pcombo 1 2 1 2
    Gtk.tableAttachDefaults table valign 0 1 2 3
    Gtk.tableAttachDefaults table ventry 1 2 2 3
    Gtk.tableAttachDefaults table idxalign 0 1 3 4
    Gtk.tableAttachDefaults table idxspin 1 2 3 4
    Gtk.tableSetRowSpacings table 10
    Gtk.tableSetColSpacings table 10
    let types = ["text", "checkbox", "radio", "textcell"]
    traverse_ (Gtk.listStoreAppend tstore) types
    Gtk.containerAdd salign hsep
    Gtk.widgetSetSensitive rem False
    Gtk.widgetSetSensitive app False
    Gtk.widgetSetSensitive pentry False
    Gtk.widgetSetSensitive pcombo False
    Gtk.boxPackStart tvbox table Gtk.PackNatural 0
    Gtk.boxPackStart tvbox aapp Gtk.PackNatural 0
    Gtk.boxPackStart vleft salign Gtk.PackNatural 0
    Gtk.boxPackStart vleft arem Gtk.PackNatural 0
    Gtk.containerAdd vleft tvbox
    Gtk.widgetSetChildVisible valign False
    Gtk.widgetSetChildVisible ventry False
    Gtk.widgetSetChildVisible idxalign False
    Gtk.widgetSetChildVisible idxspin False
    Gtk.widgetHideAll valign
    Gtk.widgetHideAll ventry
    Gtk.widgetHideAll idxalign
    Gtk.widgetHideAll idxspin

    -- Window configuration
    Gtk.set win [ Gtk.windowTitle          Gtk.:= msgStr MsgMainTitle
                , Gtk.windowDefaultWidth   Gtk.:= 800
                , Gtk.windowDefaultHeight  Gtk.:= 600
                , Gtk.containerBorderWidth Gtk.:= 10
                ]

    -- Status bar
    sbar    <- Gtk.statusbarNew
    sbalign <- Gtk.alignmentNew 0 1 1 0
    ctxId   <- Gtk.statusbarGetContextId sbar "mode"
    Gtk.statusbarSetHasResizeGrip sbar False
    Gtk.containerAdd sbalign sbar
    Gtk.boxPackEnd vbox sbalign Gtk.PackNatural 0

    Gtk.onDestroy win $ do
                Gtk.mainQuit
                appTerminate

    Gtk.widgetShowAll win

    cache  <- newIORef Nothing

    bdw <- newBlankDocumentWidget msgStr win

    return $ GUI{ guiWindow = win
                , guiPdfDialog = pdfch
                , guiJsonOpenDialog = jsonLch
                , guiJsonSaveDialog = jsonSch
                , guiPdfOpenMenuItem = oitem
                , guiOpenBlankMenuItem = ovitem
                , guiJsonOpenMenuItem = iitem
                , guiJsonSaveMenuItem = sitem
                , guiOverlapMenuItem = citem
                , guiPrevButton = prev
                , guiNextButton = next
                , guiZoomInButton = plus
                , guiZoomOutButton = minus
                , guiRemoveButton = rem
                , guiApplyButton = app
                , guiDrawToggle = drwb
                , guiDupToggle = db
                , guiMultiSelToggle = msb
                , guiVRuler = vruler
                , guiHRuler = hruler
                , guiDrawingArea = area
                , guiRectStore = store
                , guiNameEntry = pentry
                , guiValueEntry = ventry
                , guiTypeCombo = pcombo
                , guiRectTreeSelection = sel
                , guiTypeStore = tstore
                , guiValueEntryAlign = valign
                , guiWindowVBox = wvbox
                , guiWindowHBox = hbox
                , guiVRulerAdjustment = vadj
                , guiHRulerAdjustment = hadj
                , guiModeToolbar = mtoolbar
                , guiTranslate = msgStr
                , guiDokButton = akb
                , guiIndexAlign = idxalign
                , guiIndexSpin = idxspin
                , guiSplashAlign = splalign
                , guiSplashOpen = splopen
                , guiSplashDok = spldok
                , guiPdfCache = cache
                , guiContextId = ctxId
                , guiStatusBar = sbar
                , guiDrawPopup = drawpop
                , guiBlankDocumentWidget = bdw
                }

--------------------------------------------------------------------------------
createDialog :: Gtk.Window
             -> String -- title
             -> String -- file pattern
             -> String -- filter name
             -> String -- affirmative action label
             -> Gtk.FileChooserAction
             -> (DhekMessage -> String)
             -> IO Gtk.FileChooserDialog
createDialog win title pat filtName afflabel action msgStr = do
    ch   <- Gtk.fileChooserDialogNew (Just title) (Just win) action responses
    filt <- Gtk.fileFilterNew
    Gtk.fileFilterAddPattern filt pat
    Gtk.fileFilterSetName filt filtName
    Gtk.fileChooserAddFilter ch filt
    return ch
  where
    responses = [ (afflabel        , Gtk.ResponseOk)
                , (msgStr MsgCancel, Gtk.ResponseCancel)
                ]

--------------------------------------------------------------------------------
runGUI :: GUI -> IO ()
runGUI _ = Gtk.mainGUI

--------------------------------------------------------------------------------
loadImage :: Ptr (Gtk.InlineImage) -> IO Gtk.Image
loadImage = Gtk.pixbufNewFromInline >=> Gtk.imageNewFromPixbuf

--------------------------------------------------------------------------------
guiPdfSurface :: MonadIO m => PageItem -> Double -> GUI -> m Cairo.Surface
guiPdfSurface pg ratio gui
    = liftIO $
          do opt <- readIORef (guiPdfCache gui)
             let pgw = pageWidth pg  * ratio
                 pgh = pageHeight pg * ratio
                 nocache
                     = do suf <- Cairo.createImageSurface Cairo.FormatARGB32
                                 (truncate pgw) (truncate pgh)
                          Cairo.renderWith suf $
                              do Cairo.setSourceRGB 1.0 1.0 1.0
                                 Cairo.rectangle 0 0 pgw pgh
                                 Cairo.fill
                                 Cairo.scale ratio ratio
                                 Poppler.pageRender (pagePtr pg)

                          writeIORef (guiPdfCache gui) $ Just suf
                          return suf
             maybe nocache return opt

--------------------------------------------------------------------------------
guiClearPdfCache :: MonadIO m => GUI -> m ()
guiClearPdfCache gui
    = liftIO $
          do opt <- readIORef (guiPdfCache gui)
             let oncache suf
                     = do Cairo.surfaceFinish suf
                          writeIORef (guiPdfCache gui) Nothing
             maybe (return ()) oncache opt

--------------------------------------------------------------------------------
layoutMapping :: Rect -> [Gtk.AttrOp Gtk.CellRendererText]
layoutMapping r
    | r ^. rectType == "radio" =
        let value = fromMaybe "" (r ^. rectValue)
            name  = r ^. rectName
            label = name ++ " (" ++ value ++ ")" in
        [Gtk.cellText Gtk.:= label]
    | r ^. rectType == "textcell" =
        let idx   = maybe "" show (r ^. rectIndex)
            name  = r ^. rectName
            label = name ++ " (" ++ idx ++ ")" in
        [Gtk.cellText Gtk.:= label]
    | otherwise = [Gtk.cellText Gtk.:= r ^. rectName]
