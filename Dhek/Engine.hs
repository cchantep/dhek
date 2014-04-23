{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
module Dhek.Engine where

import Prelude hiding (foldr)

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens
    ( at
    , to
    , use
    , makeLenses
    , (^.)
    , (<-=)
    , (<+=)
    , (&)
    , (.~)
    , (?~)
    , (?=)
    , (.=)
    , (%~)
    , (<+~)
    , (<-~))
import Control.Monad ((<=<))
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.RWS.Strict
import Control.Monad.State

import Data.Array (Array, array, (!))
import Data.Foldable (find, foldMap, traverse_, foldr)
import qualified Data.IntMap as I
import Data.IORef
import Data.Maybe (fromJust, isNothing, isJust)
import Data.Traversable (traverse)

import Debug.Trace

import Graphics.UI.Gtk ( AttrOp( (:=) ))
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import qualified Graphics.UI.Gtk.Poppler.Page     as Poppler

import Dhek.Types
import Dhek.Utils (takeFileName, trimString)
import Dhek.Instr
import Dhek.I18N

type EngineCallback a = a -> RWST EngineEnv () EngineState IO ()

data Engine = Engine
    { _engineState         :: IORef EngineState
    , _engineEnv           :: IORef EngineEnv
    , _enginePdfSel        :: DhekProgram ()
    , _engineJsonLoad      :: DhekProgram ()
    , _engineJsonSave      :: DhekProgram ()
    , _engineNextPage      :: DhekProgram ()
    , _enginePrevPage      :: DhekProgram ()
    , _engineNextZoom      :: DhekProgram ()
    , _enginePrevZoom      :: DhekProgram ()
    , _engineRemoveRect    :: DhekProgram ()
    , _engineTreeSelection :: DhekProgram ()
    , _engineDrawing       :: DhekProgram ()
    , _engineMove          :: DhekProgram ()
    , _enginePress         :: DhekProgram ()
    , _engineRelease       :: DhekProgram ()
    , _engineEnter         :: DhekProgram ()
    , _enginePropChanged   :: DhekProgram () }

data EngineInternal = EngineInternal
    { engineRef :: IORef Viewer
    }

data EngineState = EngineState
    { _engineCurPage   :: {-# UNPACK #-} !Int
    , _engineCurZoom   :: {-# UNPACK #-} !Int
    , _engineRectId    :: {-# UNPACK #-} !Int
    , _engineOverlap   :: !Bool
    , _engineDraw      :: !Bool
    , _enginePropLabel :: !String
    , _enginePropType  :: !(Maybe String)
    , _engineEvent     :: !(Maybe BoardEvent)
    , _engineSelection :: !(Maybe Rect)
    , _engineSelected  :: !(Maybe Rect)
    , _engineCursor    :: !(Maybe Gtk.CursorType)
    , _engineAddedRect :: !(Maybe Rect)
    , _engineRemRect   :: !(Maybe Rect)
    , _enginePrevPos   :: !(Double, Double)
    , _engineColPos    :: !(Maybe (Double, Double, Double, Double, Double, Direction))
    }

data EngineEnv = EngineEnv
    { _enginePrevX     :: {-# UNPACK #-} !Double
    , _enginePrevY     :: {-# UNPACK #-} !Double
    , _enginePageCount :: {-# UNPACK #-} !Int
    , _engineFilename  :: !String
    , _engineRects     :: ![Rect]
    , _engineOverRect  :: !(Maybe Rect)
    , _engineOverArea  :: !(Maybe Area)
    }

makeLenses ''EngineState
makeLenses ''Engine

gtkEngineNew :: IO Engine
gtkEngineNew = do
    eRef <- newIORef envNew
    sRef <- newIORef sNew
    return $ Engine
        sRef
        eRef
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
  where
    nop = compile $ return ()
    envNew =
        let neg1 :: forall a. Num a => a
            neg1 = negate 1 in
        EngineEnv neg1 neg1 neg1 [] [] Nothing Nothing

    sNew = EngineState
           1
           3
           0
           False
           False
           ""
           Nothing
           Nothing
           Nothing
           Nothing
           Nothing
           Nothing
           Nothing
           (negate 1, negate 1)
           Nothing

engineStart :: Engine -> IO ()
engineStart eng = do
    Gtk.initGUI
    msgStr   <- mkI18N
    iRef     <- newIORef (error "impossible situation")
    window   <- Gtk.windowNew
    wvbox    <- Gtk.vBoxNew False 10
    fdialog  <- createPdfChooserDialog msgStr  window
    jdialog  <- createJsonChooserDialog msgStr window
    idialog  <- createJsonImportDialog msgStr window
    mbar     <- Gtk.menuBarNew
    malign   <- Gtk.alignmentNew 0 0 1 0
    fitem    <- Gtk.menuItemNewWithLabel $ msgStr MsgFile
    oitem    <- Gtk.menuItemNewWithLabel $ msgStr MsgOpenPDF
    iitem    <- Gtk.menuItemNewWithLabel $ msgStr MsgLoadMappings
    sitem    <- Gtk.menuItemNewWithLabel $ msgStr MsgSaveMappings
    citem    <- Gtk.checkMenuItemNewWithLabel $ msgStr MsgEnableOverlap
    prev     <- Gtk.buttonNewWithLabel $ msgStr MsgPrevious
    next     <- Gtk.buttonNewWithLabel $ msgStr MsgNext
    minus    <- Gtk.buttonNewWithLabel "-"
    plus     <- Gtk.buttonNewWithLabel "+"
    rem      <- Gtk.buttonNewWithLabel $ msgStr MsgRemove
    store    <- Gtk.listStoreNew ([] :: [Rect])
    treeV    <- Gtk.treeViewNewWithModel store
    sel      <- Gtk.treeViewGetSelection treeV
    fmenu    <- Gtk.menuNew
    area     <- Gtk.drawingAreaNew
    hruler   <- Gtk.hRulerNew
    halign   <- Gtk.alignmentNew 0 0 1 1
    valign   <- Gtk.alignmentNew 0 0 0 1
    hadj     <- Gtk.adjustmentNew 0 0 0 0 0 0
    vadj     <- Gtk.adjustmentNew 0 0 0 0 0 0
    viewport <- Gtk.viewportNew hadj vadj
    hscroll  <- Gtk.hScrollbarNew hadj
    vscroll  <- Gtk.vScrollbarNew vadj
    tswin    <- Gtk.scrolledWindowNew Nothing Nothing
    vbox     <- Gtk.vBoxNew False 10
    hbox     <- Gtk.hBoxNew False 10
    vleft    <- Gtk.vBoxNew False 10
    align    <- Gtk.alignmentNew 0 0 0 0
    aswin    <- Gtk.alignmentNew 0 0 1 1
    atswin   <- Gtk.alignmentNew 0 0 1 1
    arem     <- Gtk.alignmentNew 0.5 0 0 0
    bbox     <- Gtk.hButtonBoxNew
    vruler   <- Gtk.vRulerNew
    hruler   <- Gtk.hRulerNew
    pEntry   <- Gtk.entryNew
    pCombo   <- Gtk.comboBoxNew
    atable   <- Gtk.tableNew 3 3 False
    nlabel   <- Gtk.labelNew (Just $ msgStr MsgName)
    tlabel   <- Gtk.labelNew (Just $ msgStr MsgType)
    salign   <- Gtk.alignmentNew 0 0 1 0
    ualign   <- Gtk.alignmentNew 0.5 0 0 0
    nalign   <- Gtk.alignmentNew 0 0.5 0 0
    talign   <- Gtk.alignmentNew 0 0.5 0 0
    cstore   <- Gtk.comboBoxSetModelText pCombo
    table    <- Gtk.tableNew 2 2 False
    tvbox    <- Gtk.vBoxNew False 10
    vsep     <- Gtk.vSeparatorNew
    hsep     <- Gtk.hSeparatorNew
    Gtk.containerAdd viewport area
    Gtk.set vruler [Gtk.rulerMetric := Gtk.Pixels]
    Gtk.set hruler [Gtk.rulerMetric := Gtk.Pixels]
    Gtk.menuShellAppend fmenu oitem
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
    Gtk.widgetAddEvents area [Gtk.PointerMotionMask]
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
    Gtk.containerAdd arem rem
    Gtk.containerAdd align bbox
    Gtk.containerAdd bbox prev
    Gtk.containerAdd bbox next
    Gtk.containerAdd bbox vsep
    Gtk.containerAdd bbox minus
    Gtk.containerAdd bbox plus
    Gtk.boxPackStart vbox align Gtk.PackNatural 0
    Gtk.containerAdd atswin tswin
    Gtk.boxPackStart vleft atswin Gtk.PackGrow 0
    Gtk.boxPackStart vleft arem Gtk.PackNatural 0
    Gtk.boxPackStart vbox atable Gtk.PackGrow 0
    Gtk.boxPackStart hbox vbox Gtk.PackGrow 0
    Gtk.boxPackStart hbox vleft Gtk.PackNatural 0
    col <- Gtk.treeViewColumnNew
    Gtk.treeViewColumnSetTitle col $ msgStr $ MsgAreas
    trenderer <- Gtk.cellRendererTextNew
    Gtk.cellLayoutPackStart col trenderer False
    let mapping r = [Gtk.cellText := r ^. rectName]
    Gtk.cellLayoutSetAttributes col trenderer store mapping
    Gtk.treeViewAppendColumn treeV col
    Gtk.scrolledWindowAddWithViewport tswin treeV
    Gtk.scrolledWindowSetPolicy tswin Gtk.PolicyAutomatic Gtk.PolicyAutomatic
    -- Properties --
    nlabel <- Gtk.labelNew (Just $ msgStr MsgName)
    tlabel <- Gtk.labelNew (Just $ msgStr MsgType)
    ualign <- Gtk.alignmentNew 0.5 0 0 0
    nalign <- Gtk.alignmentNew 0 0.5 0 0
    talign <- Gtk.alignmentNew 0 0.5 0 0
    tstore <- Gtk.comboBoxSetModelText pCombo
    table  <- Gtk.tableNew 2 2 False
    tvbox  <- Gtk.vBoxNew False 10
    Gtk.containerAdd nalign nlabel
    Gtk.containerAdd talign tlabel
    Gtk.tableAttachDefaults table nalign 0 1 0 1
    Gtk.tableAttachDefaults table pEntry 1 2 0 1
    Gtk.tableAttachDefaults table talign 0 1 1 2
    Gtk.tableAttachDefaults table pCombo 1 2 1 2
    Gtk.tableSetRowSpacings table 10
    Gtk.tableSetColSpacings table 10
    traverse_ (Gtk.listStoreAppend tstore) ["text", "checkbox", "radio"]
    Gtk.containerAdd salign hsep
    Gtk.boxPackStart tvbox table Gtk.PackNatural 0
    Gtk.boxPackStart vleft salign Gtk.PackNatural 0
    Gtk.containerAdd vleft tvbox
    Gtk.widgetSetSensitive rem False
    Gtk.widgetSetSensitive pEntry False
    Gtk.widgetSetSensitive pCombo False
    let envRef   = _engineEnv eng
        stateRef = _engineState eng
        fPdf     = _enginePdfSel eng
        jsonLF   = _engineJsonLoad eng
        nextPF   = _engineNextPage eng
        prevPF   = _enginePrevPage eng
        minusPF  = _enginePrevZoom eng
        plusPF   = _engineNextZoom eng
        remF     = _engineRemoveRect eng
        selF     = _engineTreeSelection eng
        drawingF = _engineDrawing eng
        moveF    = _engineMove eng
        pressF   = _enginePress eng
        releaseF = _engineRelease eng
        enterF   = _engineEnter eng
        propF    = _enginePropChanged eng
        saveF    = _engineJsonSave eng

        interpret :: Double -> Double -> DhekProgram () -> IO ()
        interpret x' y' pgr = do
            s     <- readIORef stateRef
            v     <- readIORef iRef
            env   <- readIORef envRef
            frame <- Gtk.widgetGetDrawWindow area
            let ratio = getRatio s v
                x     = x' / ratio
                y     = y' / ratio
                rects = getRects s v
                pId   = s ^. engineCurPage
                nb    = v ^. viewerPageCount
                page  = getPage s v
                oOpt  = getOverRect x y rects
                aOpt  = getOverArea ratio x y =<< oOpt
                filename = _engineFilename env
                pred r x = (x ^. rectId) == (r ^. rectId)

                suspend (GetPointer k)  s v = k (x,y) s v
                suspend (GetOverRect k) s v = k oOpt s v
                suspend (GetOverArea k) s v = k aOpt s v
                suspend (GetSelected k) s v = k (s ^. engineSelected) s v
                suspend (SetSelected rOpt k) s v = do
                    let callback r = do
                            let id = r ^. rectId
                                s1 = s & engineSelected ?~ r
                                v1 = v & viewerBoards.boardsMap.at pId.traverse.boardRects.at id ?~ r
                            iOpt <- lookupStoreIter (pred r) store
                            traverse_ (\it -> do
                                            let idx = Gtk.listStoreIterToIndex it
                                            Gtk.listStoreSetValue store idx r
                                            Gtk.treeSelectionSelectIter sel it
                                      ) iOpt
                            Gtk.entrySetText pEntry (r ^. rectName)
                            tOpt <- lookupStoreIter (\x -> x == (r ^. rectType)) tstore
                            writeIORef stateRef s1 -- combobox hack in order to prevent sync issue
                            traverse_ (Gtk.comboBoxSetActiveIter pCombo) tOpt
                            Gtk.widgetSetSensitive rem True
                            Gtk.widgetSetSensitive pCombo True
                            Gtk.widgetSetSensitive pEntry True
                            k s1 v1

                        onNothing =
                            suspend (UnselectRect k) s v
                    maybe onNothing callback rOpt
                suspend (GetEvent k) s v   = k (s ^. engineEvent) s v
                suspend (SetEvent e k) s v =
                    let s1 = s & engineEvent .~ e in
                    k s1 v
                suspend (GetRects k) s v = k rects s v
                suspend (GetRatio k) s v = k ratio s v
                suspend (GetSelection k) s v = k (s ^. engineSelection) s v
                suspend (SetSelection r k) s v =
                    let s1 = s & engineSelection .~ r in
                    k s1 v
                suspend (GetPage k) s v = k page s v
                suspend (GetCurPage k) s v = k (s ^. engineCurPage) s v
                suspend (GetPageCount k ) s v = k nb s v
                suspend (SetCursor tOpt k) s v = do
                    cOpt <- traverse Gtk.cursorNew tOpt
                    Gtk.drawWindowSetCursor frame cOpt
                    k s v
                suspend (FreshId k) s v =
                    let (id, s1) = s & engineRectId <+~ 1 in
                    k id s1 v
                suspend (IncrPage k) s v = do
                    let (ncur, s1) = s & engineSelected .~ Nothing
                                       & engineCurPage <+~ 1
                        rects2     = getRects s1 v
                    Gtk.widgetSetSensitive prev True
                    Gtk.widgetSetSensitive next (ncur < nb)
                    Gtk.listStoreClear store
                    traverse_ (Gtk.listStoreAppend store) rects2
                    k s1 v
                suspend (DecrPage k) s v = do
                    let (ncur, s1) = s & engineSelected .~ Nothing
                                       & engineCurPage <-~ 1
                        rects2     = getRects s1 v
                    Gtk.widgetSetSensitive prev (ncur > 1)
                    Gtk.widgetSetSensitive next True
                    Gtk.listStoreClear store
                    traverse_ (Gtk.listStoreAppend store) rects2
                    k s1 v
                suspend (IncrZoom k) s v = do
                    let (ncur, s1) = s & engineCurZoom <+~ 1
                    Gtk.widgetSetSensitive minus True
                    Gtk.widgetSetSensitive plus (ncur < 10)
                    k s1 v
                suspend (DecrZoom k) s v = do
                    let (ncur, s1) = s & engineCurZoom <-~ 1
                    Gtk.widgetSetSensitive minus (ncur > 1)
                    Gtk.widgetSetSensitive plus True
                    k s1 v
                suspend (RemoveRect r k) s v = do
                    let id = r ^. rectId
                        v1 = v & viewerBoards.boardsMap.at pId.traverse.boardRects.at id .~ Nothing
                    iOpt <- lookupStoreIter (pred r) store
                    traverse_ (\it ->
                                let idx = Gtk.listStoreIterToIndex it in
                                Gtk.listStoreRemove store idx
                              ) iOpt
                    k s v1
                suspend (DetachRect r k) s v = do
                    let id = r ^. rectId
                        v1 = v & viewerBoards.boardsMap.at pId.traverse.boardRects.at id .~ Nothing
                    k s v1
                suspend (AttachRect r k) s v = do
                    let id = r ^. rectId
                        v1 = v & viewerBoards.boardsMap.at pId.traverse.boardRects.at id ?~ r
                    k s v1
                suspend (AddRect r k) s v = do
                    let id = r ^. rectId
                        v1 = v & viewerBoards.boardsMap.at pId.traverse.boardRects.at id ?~ r
                    Gtk.listStoreAppend store r
                    iOpt <- lookupStoreIter (pred r) store
                    traverse_ (Gtk.treeSelectionSelectIter sel) iOpt
                    k s v1
                suspend (GetEntryText e k) s v =
                    case e of
                        PropEntry -> do
                            txt <- Gtk.entryGetText pEntry
                            let txt1 = trimString txt
                                tOpt = if null txt1 then Nothing else Just txt1
                            k tOpt s v
                suspend (GetComboText e k) s v =
                    case e of
                        PropCombo -> do
                            tOpt <- Gtk.comboBoxGetActiveText pCombo
                            k tOpt s v
                suspend (UnselectRect k) s v = do
                    let s1 = s & engineSelected .~ Nothing
                    Gtk.widgetSetSensitive pCombo False
                    Gtk.widgetSetSensitive pEntry False
                    Gtk.widgetSetSensitive rem False
                    Gtk.entrySetText pEntry ""
                    Gtk.comboBoxSetActive pCombo (negate 1)
                    k s1 v
                suspend (Draw k) s v =
                    let s1 = s & engineDraw .~ True in
                    k s1 v
                suspend (SetTitle t k) s v = do
                    Gtk.windowSetTitle window t
                    k s v
                suspend (GetFilename k) s v = k filename s v
                suspend (GetFrameSize k) s v = do
                    size <- Gtk.drawableGetSize frame
                    k size s v
                suspend (ExecCairo r k) s v = do
                    Gtk.renderWithDrawable frame r
                    k s v
                suspend (SizeRequest rx ry k) s v = do
                    Gtk.widgetSetSizeRequest area rx ry
                    k s v
                suspend (ShowError e k) s v = do
                    m <- Gtk.messageDialogNew (Just window)
                         [Gtk.DialogModal] Gtk.MessageError Gtk.ButtonsOk e
                    Gtk.dialogRun m
                    Gtk.widgetHide m
                    k s v
                suspend (PerformIO action k) s v = do
                    b <- action
                    k b s v
                suspend (GetTreeSelection k) s v = do
                    iOpt <- Gtk.treeSelectionGetSelected sel
                    rOpt <- traverse (\it ->
                                       let idx = Gtk.listStoreIterToIndex it in
                                       Gtk.listStoreGetValue store idx
                                     ) iOpt
                    k rOpt s v
                suspend (NewGuide t k) s v =
                    let v1 = v & viewerBoards.boardsCurGuide ?~ Guide 0 t in
                    k s v1
                suspend (UpdateGuide k) s v = do
                    x <- Gtk.get hruler Gtk.rulerPosition
                    y <- Gtk.get vruler Gtk.rulerPosition
                    let upd g =
                            let v = case g ^. guideType of
                                    GuideVertical   -> x
                                    GuideHorizontal -> y in
                            g & guideValue .~ v
                        v1 = v & viewerBoards.boardsCurGuide %~ fmap upd
                    k s v1
                suspend (AddGuide k) s v =
                    let action = do
                            gOpt <- use $ viewerBoards.boardsCurGuide
                            gs   <- use $ viewerBoards.boardsGuides
                            let gs1 = foldr (:) gs gOpt
                            viewerBoards.boardsCurGuide .= Nothing
                            viewerBoards.boardsGuides   .= gs1
                        v1 = execState action v in
                    k s v1
                suspend (GetCurGuide k) s v =
                    k (v ^. viewerBoards.boardsCurGuide) s v
                suspend (GetGuides k) s v =
                    k (v ^. viewerBoards.boardsGuides) s v
                suspend (SelectJsonFile k) s v = do
                    resp <- Gtk.dialogRun jdialog
                    Gtk.widgetHide jdialog
                    case resp of
                        Gtk.ResponseCancel -> k Nothing s v
                        Gtk.ResponseOk     -> do
                            fOpt <- Gtk.fileChooserGetFilename jdialog
                            k fOpt s v
                suspend (GetAllRects k) s v =
                    let tup (i, b) = (i, b ^. boardRects.to I.elems)
                        list       = fmap tup . I.toList in
                    k (v ^. viewerBoards.boardsMap.to list) s v
                suspend (OpenJsonFile k) s v = do
                    resp <- Gtk.dialogRun idialog
                    Gtk.widgetHide idialog
                    case resp of
                        Gtk.ResponseCancel -> k Nothing s v
                        Gtk.ResponseOk     -> do
                            fOpt <- Gtk.fileChooserGetFilename idialog
                            k fOpt s v
                suspend (SetRects xs k) s v = do
                    let onEach page r = do
                            id <- boardsState <+= 1
                            let r1 = r & rectId .~ id
                            boardsMap.at page.traverse.boardRects.at id ?= r1

                        go (page, rs) = traverse_ (onEach page) rs
                        action        = traverse_ go xs
                        nb            = length xs
                        b             = execState action (boardsNew nb)
                        v1            = v & viewerBoards .~ b
                        s1            = s & engineRectId .~ (b ^. boardsState)
                        rects2        = getRects s1 v1
                    Gtk.listStoreClear store
                    traverse_ (Gtk.listStoreAppend store) rects2
                    k s1 v1
                suspend (Active o b k) s v =
                    case o of
                        Overlap -> do
                            let s1 = s & engineOverlap .~ b
                            Gtk.checkMenuItemSetActive citem b
                            k s1 v
                suspend (IsActive o k) s v =
                    case o of
                        Overlap -> k (s ^. engineOverlap) s v
                suspend (PrevPointer k) s v =
                    k (s ^. enginePrevPos) s v
                suspend (SetCol o k) s v =
                    let s1 = s & engineColPos .~ o in
                    k s1 v
                suspend (GetCol k) s v =
                    k (s ^. engineColPos) s v

                end a s v = do
                    let drawing = s ^. engineDraw
                        s1      = s & engineDraw    .~ False
                                    & enginePrevPos .~ (x,y)
                    writeIORef iRef v
                    writeIORef stateRef s1
                    when drawing (Gtk.widgetQueueDraw area)

            (foldFree end suspend pgr) s v
            where
              getRatio :: EngineState -> Viewer -> Double
              getRatio s v = (base * zoom) / width
                where
                  pIdx  = _engineCurPage s
                  zIdx  = _engineCurZoom s
                  pages = _viewerPages v
                  base  = 777
                  width = pageWidth (pages ! pIdx)
                  zoom  = zoomValues ! zIdx

              getPage :: EngineState -> Viewer -> PageItem
              getPage s v = pages ! pIdx
                where
                  pIdx  = _engineCurPage s
                  pages = _viewerPages v

              getOverRect :: Double -> Double -> [Rect] -> Maybe Rect
              getOverRect x y rs =
                  let overed = isOver 1.0 x y
                      oOpt   = find overed rs in
                  oOpt

              getOverArea :: Double -> Double -> Double -> Rect -> Maybe Area
              getOverArea ratio x y r =
                  let overed a =
                          isOver 1.0 x y (rectArea (5/ratio) r a)

                      aOpt = find overed (enumFrom TOP_LEFT) in
                  aOpt

              getRects :: EngineState -> Viewer -> [Rect]
              getRects s v =
                  let pId   = s ^. engineCurPage
                      rects =
                          v ^. viewerBoards.boardsMap.at pId.traverse.boardRects.to I.elems in
                  rects

    Gtk.on oitem Gtk.menuItemActivate $ do
        resp <- Gtk.dialogRun fdialog
        Gtk.widgetHide fdialog
        case resp of
            Gtk.ResponseCancel -> return ()
            Gtk.ResponseOk     -> do
                uriOpt  <- Gtk.fileChooserGetURI fdialog
                nameOpt <- Gtk.fileChooserGetFilename fdialog
                iOpt    <- traverse makeInternal uriOpt
                s       <- readIORef stateRef
                traverse (writeIORef iRef) iOpt
                v       <- readIORef iRef
                let env  = initEnv nameOpt v
                    name = _engineFilename env
                writeIORef envRef env
                let nb     = v ^. viewerPageCount
                    s'     = initState v s
                writeIORef stateRef s'
                ahbox <- Gtk.alignmentNew 0 0 1 1
                Gtk.containerAdd ahbox hbox
                Gtk.boxPackStart wvbox ahbox Gtk.PackGrow 0
                Gtk.widgetSetSensitive oitem False
                Gtk.widgetSetSensitive iitem True
                Gtk.widgetSetSensitive sitem True
                Gtk.widgetSetSensitive citem True
                Gtk.widgetSetSensitive prev False
                Gtk.widgetSetSensitive next (nb /= 1)
                Gtk.windowSetTitle window
                        (name ++ " (page 1 / " ++ show nb ++ ")")
                Gtk.widgetShowAll ahbox
    Gtk.on iitem Gtk.menuItemActivate $ do
        let x = negate 1
        interpret x x jsonLF
    Gtk.on sitem Gtk.menuItemActivate $ do
        let x = negate 1
        interpret x x saveF
    Gtk.on citem Gtk.menuItemActivate $ do
        let x      = negate 1
            action = compile $ do
                b <- isActive Overlap
                active Overlap (not b)
        interpret x x action
    -- Previous Button ---
    Gtk.on prev Gtk.buttonActivated $ do
        let x = negate 1
        interpret x x prevPF
    --- Next Button ---
    Gtk.on next Gtk.buttonActivated $ do
        let x = negate 1
        interpret x x nextPF
    --- Minus Button ---
    Gtk.on minus Gtk.buttonActivated $ do
        let x = negate 1
        interpret x x minusPF
    --- Plus Button ---
    Gtk.on plus Gtk.buttonActivated $ do
        let x = negate 1
        interpret x x plusPF
    Gtk.on rem Gtk.buttonActivated $ do
        let x = negate 1
        interpret x x remF
    --- Selection ---
    Gtk.on sel Gtk.treeSelectionSelectionChanged $ do
        let x = negate 1
        interpret x x selF
    Gtk.on area Gtk.exposeEvent $ Gtk.tryEvent $ liftIO $ do
        let x = negate 1
            action = compile $ do
                ratio <- getRatio
                performIO $ do
                    hsize  <- Gtk.adjustmentGetPageSize hadj
                    vsize  <- Gtk.adjustmentGetPageSize vadj
                    hlower <- Gtk.adjustmentGetLower hadj
                    hupper <- Gtk.adjustmentGetUpper hadj
                    hincr  <- Gtk.adjustmentGetPageIncrement hadj
                    sincr  <- Gtk.adjustmentGetStepIncrement hadj
                    vincr  <- Gtk.adjustmentGetPageIncrement vadj
                    hvalue <- Gtk.adjustmentGetValue hadj
                    vvalue <- Gtk.adjustmentGetValue vadj
                    let w  = (hsize/ratio) + hl
                        h  = (vsize/ratio) + vl
                        hl = hvalue / ratio
                        vl = vvalue / ratio
                    Gtk.set hruler [ Gtk.rulerLower   := hl
                                   , Gtk.rulerUpper   := w
                                   , Gtk.rulerMaxSize := w]
                    Gtk.set vruler [ Gtk.rulerLower   := vl
                                   , Gtk.rulerUpper   := h
                                   , Gtk.rulerMaxSize := h]
        interpret x x (drawingF >> action)
    Gtk.on area Gtk.motionNotifyEvent $ Gtk.tryEvent $ do
        (x',y') <- Gtk.eventCoordinates
        let action = compile $ do
                (x,y) <- getPointer
                performIO $ do
                    Gtk.set hruler [Gtk.rulerPosition := x]
                    Gtk.set vruler [Gtk.rulerPosition := y]

        liftIO $ interpret x' y' (moveF >> action)
    Gtk.on area Gtk.buttonPressEvent $ Gtk.tryEvent $ do
       (x',y') <- Gtk.eventCoordinates
       b       <- Gtk.eventButton
       when (b == Gtk.LeftButton) $
           liftIO $ interpret x' y' pressF
    Gtk.on area Gtk.buttonReleaseEvent $ Gtk.tryEvent $ liftIO $ do
        let x = negate 1
        interpret x x releaseF
    Gtk.on area Gtk.enterNotifyEvent $ Gtk.tryEvent $ liftIO $ do
        let x = negate 1
        interpret x x enterF
    Gtk.on area Gtk.scrollEvent $ Gtk.tryEvent $ do
        dir <- Gtk.eventScrollDirection
        liftIO $ do
            pageSize <- Gtk.adjustmentGetPageSize vadj
            lower    <- Gtk.adjustmentGetLower vadj
            upper    <- Gtk.adjustmentGetUpper vadj
            step     <- Gtk.adjustmentGetStepIncrement vadj
            oldValue <- Gtk.adjustmentGetValue vadj
            let delta' = step * 2
                delta  = case dir of
                    Gtk.ScrollUp -> negate delta'
                    _            -> delta'
                newValue = min (upper - pageSize) (max 0 (oldValue + delta))
            Gtk.adjustmentSetValue vadj newValue
    Gtk.on pEntry Gtk.entryActivate $ do
       let x = negate 1
       interpret x x propF
    Gtk.on pCombo Gtk.changed $ do
       let x = negate 1
       interpret x x propF
    Gtk.on vruler Gtk.buttonPressEvent $ Gtk.tryEvent $ do
        let x      = negate 1
            action = compile $ newGuide GuideVertical
        liftIO $ interpret x x action
    Gtk.on vruler Gtk.motionNotifyEvent $ Gtk.tryEvent $ do
        (x',y') <- Gtk.eventCoordinates
        let x      = negate 1
            action = compile $ do
                ratio <- getRatio
                performIO $ do
                    v <- Gtk.adjustmentGetValue hadj
                    let (x,y) = ((x'-25+v)/ratio, y'/ratio)
                    Gtk.set hruler [Gtk.rulerPosition := x]
                    Gtk.set vruler [Gtk.rulerPosition := y]
                updateGuide
                draw
        liftIO $ interpret x' y' action
    Gtk.on vruler Gtk.buttonReleaseEvent $ Gtk.tryEvent $ liftIO $ do
        let x      = negate 1
            action = compile $ do
                addGuide
                draw
        liftIO $ interpret x x action
    Gtk.on hruler Gtk.buttonPressEvent $ Gtk.tryEvent $ do
        let x      = negate 1
            action = compile $ newGuide GuideHorizontal
        liftIO $ interpret x x action
    Gtk.on hruler Gtk.motionNotifyEvent $ Gtk.tryEvent $ do
        (x',y') <- Gtk.eventCoordinates
        let x      = negate 1
            action = compile $ do
                ratio <- getRatio
                performIO $ do
                    v <- Gtk.adjustmentGetValue vadj
                    let (x,y) = (x'/ratio, (y'+v-25)/ratio)
                    Gtk.set hruler [Gtk.rulerPosition := x]
                    Gtk.set vruler [Gtk.rulerPosition := y]
                updateGuide
                draw
        liftIO $ interpret x' y' action
    Gtk.on hruler Gtk.buttonReleaseEvent $ Gtk.tryEvent $ liftIO $ do
        let x      = negate 1
            action = compile $ do
                addGuide
                draw
        liftIO $ interpret x x action
    Gtk.containerAdd window wvbox
    Gtk.set window (windowParams msgStr)
    Gtk.onDestroy window Gtk.mainQuit
    Gtk.widgetShowAll window
    Gtk.mainGUI
  where
    makeInternal uri = do
        v <- loadPdf uri
        return v

createPdfChooserDialog :: (DhekMessage -> String) -> Gtk.Window -> IO Gtk.FileChooserDialog
createPdfChooserDialog msgStr win = do
  ch   <- Gtk.fileChooserDialogNew title
          (Just win) Gtk.FileChooserActionOpen responses
  filt <- Gtk.fileFilterNew
  Gtk.fileFilterAddPattern filt "*.pdf"
  Gtk.fileFilterSetName filt $ msgStr MsgPdfFilter
  Gtk.fileChooserAddFilter ch filt
  return ch
    where
      responses = [(msgStr MsgOpen, Gtk.ResponseOk)
                  ,(msgStr MsgCancel, Gtk.ResponseCancel)]
      title = Just $ msgStr MsgOpenPDF

createJsonChooserDialog :: (DhekMessage -> String) -> Gtk.Window -> IO Gtk.FileChooserDialog
createJsonChooserDialog msgStr win = do
  ch   <- Gtk.fileChooserDialogNew title (Just win)
          Gtk.FileChooserActionSave responses
  filt <- Gtk.fileFilterNew
  Gtk.fileFilterAddPattern filt "*.json"
  Gtk.fileFilterSetName filt $ msgStr MsgJsonFilter
  Gtk.fileChooserAddFilter ch filt
  Gtk.fileChooserSetDoOverwriteConfirmation ch True
  return ch
    where
      responses = [(msgStr MsgSave, Gtk.ResponseOk)
                  ,(msgStr MsgCancel, Gtk.ResponseCancel)]
      title = Just $ msgStr MsgJsonFilter

createJsonImportDialog :: (DhekMessage -> String) -> Gtk.Window -> IO Gtk.FileChooserDialog
createJsonImportDialog msgStr win = do
  ch <- Gtk.fileChooserDialogNew title (Just win)
        Gtk.FileChooserActionOpen responses
  filt <- Gtk.fileFilterNew
  Gtk.fileFilterAddPattern filt "*.json"
  Gtk.fileFilterSetName filt $ msgStr MsgJsonFilter
  Gtk.fileChooserAddFilter ch filt
  return ch
    where
      responses = [(msgStr MsgChoose, Gtk.ResponseOk)
                  ,(msgStr MsgCancel, Gtk.ResponseCancel)]
      title = Just $ msgStr MsgChooseJSON

windowParams :: (DhekMessage -> String) -> [Gtk.AttrOp Gtk.Window]
windowParams msgStr =
    [Gtk.windowTitle          := msgStr MsgMainTitle
    ,Gtk.windowDefaultWidth   := 800
    ,Gtk.windowDefaultHeight  := 600
    ,Gtk.containerBorderWidth := 10]

loadPdf :: FilePath -> IO Viewer
loadPdf path = do
  doc   <- fmap fromJust (Poppler.documentNewFromFile path Nothing)
  nb    <- Poppler.documentGetNPages doc
  pages <- loadPages doc
  return (Viewer doc pages 1 nb 100 3 1.0 (boardsNew nb))

lookupStoreIter :: (a -> Bool) -> Gtk.ListStore a -> IO (Maybe Gtk.TreeIter)
lookupStoreIter pred store = Gtk.treeModelGetIterFirst store >>= go
  where
    go (Just it) = do
        a <- Gtk.listStoreGetValue store (Gtk.listStoreIterToIndex it)
        if pred a
            then return (Just it)
            else Gtk.treeModelIterNext store it >>= go
    go _ = return Nothing

loadPages :: Poppler.Document -> IO (Array Int PageItem)
loadPages doc = do
    nb <- Poppler.documentGetNPages doc
    fmap (array (1,nb)) (traverse go [1..nb])
  where
    go i = do
        page  <- Poppler.documentGetPage doc (i-1)
        (w,h) <- Poppler.pageGetSize page
        return (i, PageItem page w h)

initEnv :: Maybe String -> Viewer -> EngineEnv
initEnv fOpt v = EngineEnv
                 0
                 0
                 (v ^. viewerPageCount)
                 (maybe "" takeFileName fOpt)
                 []
                 Nothing
                 Nothing

initState :: Viewer -> EngineState -> EngineState
initState v s = EngineState
                1
                3
                0
                (s ^. engineOverlap)
                False
                ""
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                (negate 1, negate 1)
                Nothing

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

handCursor :: Gtk.CursorType
handCursor = Gtk.Hand1

areaCursor :: Area -> Gtk.CursorType
areaCursor TOP_LEFT     = Gtk.TopLeftCorner
areaCursor TOP          = Gtk.TopSide
areaCursor TOP_RIGHT    = Gtk.TopRightCorner
areaCursor RIGHT        = Gtk.RightSide
areaCursor BOTTOM_RIGHT = Gtk.BottomRightCorner
areaCursor BOTTOM       = Gtk.BottomSide
areaCursor BOTTOM_LEFT  = Gtk.BottomLeftCorner
areaCursor LEFT         = Gtk.LeftSide

eventCursor :: BoardEvent -> Gtk.CursorType
eventCursor (Hold _ _)     = Gtk.Hand1
eventCursor (Resize _ _ a) = areaCursor a
