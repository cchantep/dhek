{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Dhek.Engine where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad ((<=<))
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.RWS.Strict
import Control.Monad.State

import Data.Array (Array, array, (!))
import Data.Foldable (traverse_)
import Data.IORef
import Data.Maybe (fromJust)
import Data.Traversable (traverse)

import Graphics.UI.Gtk ( AttrOp( (:=) ))
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import qualified Graphics.UI.Gtk.Poppler.Page     as Poppler

import Dhek.Types

type EngineCallback a = a -> RWST EngineEnv () EngineState IO ()

data Engine = Engine
    { _engineState        :: IORef EngineState
    , _engineEnv          :: IORef EngineEnv
    , _enginePdfSel       :: EngineCallback PdfSelection
    , _engineJsonLoad     :: EngineCallback JsonLoad
    , _engineNextPage     :: EngineCallback NextPage
    , _enginePrevPage     :: EngineCallback PrevPage
    , _engineNextZoom     :: EngineCallback NextZoom
    , _enginePrevZoom     :: EngineCallback PrevZoom
    , _engineRemoveRect   :: EngineCallback RemoveRect
    , _engineRectSelected :: EngineCallback RectSelected
    , _engineDrawing      :: EngineCallback Drawing
    , _engineMove         :: EngineCallback Move
    , _enginePress        :: EngineCallback Press
    , _engineRelease      :: EngineCallback Release
    , _engineEnter        :: EngineCallback Enter }

data EngineInternal = EngineInternal
    { engineRef :: IORef Viewer
    }

data PdfSelection = PdfSelection
    { pdfURI      :: !String
    , pdfFilename :: !String }

data JsonLoad = JsonLoad { jsonURI :: !String }

data NextPage = NextPage
data PrevPage = PrevPage

data NextZoom = NextZoom
data PrevZoom = PrevZoom

data RemoveRect = RemoveRect !Rect

data RectSelected = RectSelected !Rect

data Drawing = Drawing

data Move = Move !Double !Double
data Press = Press !Double !Double
data Release = Release

data Enter = Enter

data EngineState = EngineState
    { _engineCurPage   :: {-# UNPACK #-} !Int
    , _engineCurZoom   :: {-# UNPACK #-} !Int
    , _engineCollision :: !Bool
    , _engineEvent     :: !(Maybe BoardEvent)
    }

data EngineEnv = EngineEnv
    { _enginePrevX    :: {-# UNPACK #-} !Double
    , _enginePrevY    :: {-# UNPACK #-} !Double
    , _engineOverRect :: !(Maybe Rect)
    , _engineOverArea :: !(Maybe Area)
    , _engineSelected :: !(Maybe Rect)
    }

gtkEngineNew :: IO Engine
gtkEngineNew = do
    eRef <- newIORef envNew
    sRef <- newIORef sNew
    return $ Engine
        sRef
        eRef
        (\_ -> return ())
        (\_ -> return ())
        (\_ -> return ())
        (\_ -> return ())
        (\_ -> return ())
        (\_ -> return ())
        (\_ -> return ())
        (\_ -> return ())
        (\_ -> return ())
        (\_ -> return ())
        (\_ -> return ())
        (\_ -> return ())
        (\_ -> return ())
  where
    envNew =
        let neg1 = negate 1 in
        EngineEnv neg1 neg1 Nothing Nothing Nothing

    sNew = EngineState 1 3 True Nothing

engineStart :: Engine -> IO ()
engineStart eng = do
    Gtk.initGUI
    iRef <- newIORef (error "impossible situation")
    window  <- Gtk.windowNew
    wvbox    <- Gtk.vBoxNew False 10
    fdialog <- createPdfChooserDialog window
    jdialog <- createJsonChooserDialog window
    mbar    <- Gtk.menuBarNew
    malign  <- Gtk.alignmentNew 0 0 1 0
    fitem   <- Gtk.menuItemNewWithLabel "File"
    oitem   <- Gtk.menuItemNewWithLabel "Open PDF"
    iitem   <- Gtk.menuItemNewWithLabel "Load mappings"
    sitem   <- Gtk.menuItemNewWithLabel "Save mappings"
    prev    <- Gtk.buttonNewWithLabel "Previous"
    next    <- Gtk.buttonNewWithLabel "Next"
    minus   <- Gtk.buttonNewWithLabel "-"
    plus    <- Gtk.buttonNewWithLabel "+"
    rem     <- Gtk.buttonNewWithLabel "Remove"
    store   <- Gtk.listStoreNew ([] :: [Rect])
    treeV   <- Gtk.treeViewNewWithModel store
    sel     <- Gtk.treeViewGetSelection treeV
    fmenu   <- Gtk.menuNew
    area    <- Gtk.drawingAreaNew
    hruler  <- Gtk.hRulerNew
    halign  <- Gtk.alignmentNew 0 0 1 1
    valign  <- Gtk.alignmentNew 0 0 0 1
    hadj    <- Gtk.adjustmentNew 0 0 0 0 0 0
    vadj    <- Gtk.adjustmentNew 0 0 0 0 0 0
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
    vruler <- Gtk.vRulerNew
    hruler <- Gtk.hRulerNew
    propNameEntry <- Gtk.entryNew
    propTypeCombo <- Gtk.comboBoxNew
    atable <- Gtk.tableNew 3 3 False
    nlabel <- Gtk.labelNew (Just "Name")
    tlabel <- Gtk.labelNew (Just "Type")
    salign <- Gtk.alignmentNew 0 0 1 0
    ualign <- Gtk.alignmentNew 0.5 0 0 0
    nalign <- Gtk.alignmentNew 0 0.5 0 0
    talign <- Gtk.alignmentNew 0 0.5 0 0
    cstore  <- Gtk.comboBoxSetModelText propTypeCombo
    table  <- Gtk.tableNew 2 2 False
    tvbox  <- Gtk.vBoxNew False 10
    sep    <- Gtk.hSeparatorNew
    Gtk.set vruler [Gtk.rulerMetric := Gtk.Pixels]
    Gtk.set hruler [Gtk.rulerMetric := Gtk.Pixels]
    Gtk.menuShellAppend fmenu oitem
    Gtk.menuShellAppend fmenu iitem
    Gtk.menuShellAppend fmenu sitem
    Gtk.menuItemSetSubmenu fitem fmenu
    Gtk.menuShellAppend mbar fitem
    Gtk.containerAdd malign mbar
    Gtk.widgetSetSensitive iitem False
    Gtk.widgetSetSensitive sitem False
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
    Gtk.containerAdd bbox sep
    Gtk.containerAdd bbox minus
    Gtk.containerAdd bbox plus
    Gtk.boxPackStart vbox align Gtk.PackNatural 0
    Gtk.containerAdd atswin tswin
    Gtk.boxPackStart vleft atswin Gtk.PackGrow 0
    Gtk.boxPackStart vleft arem Gtk.PackNatural 0
    Gtk.boxPackStart vbox atable Gtk.PackGrow 0
    Gtk.boxPackStart hbox vbox Gtk.PackGrow 0
    Gtk.boxPackStart hbox vleft Gtk.PackNatural 0
    let envRef   = _engineEnv eng
        stateRef = _engineState eng
        fPdf     = _enginePdfSel eng
        jsonLF   = _engineJsonLoad eng
        nextPF   = _engineNextPage eng
        prevPF   = _enginePrevPage eng
        minusPF  = _enginePrevZoom eng
        plusPF   = _engineNextZoom eng
        remF     = _engineRemoveRect eng
        selF     = _engineRectSelected eng
        drawingF = _engineDrawing eng
        moveF    = _engineMove eng
        pressF   = _enginePress eng
        releaseF = _engineRelease eng
        enterF   = _engineEnter eng

    Gtk.on oitem Gtk.menuItemActivate $ do
        resp <- Gtk.dialogRun fdialog
        Gtk.widgetHide fdialog
        case resp of
            Gtk.ResponseCancel -> return ()
            Gtk.ResponseOk -> do
                uriOpt  <- Gtk.fileChooserGetURI fdialog
                nameOpt <- Gtk.fileChooserGetFilename fdialog
                iOpt    <- traverse makeInternal uriOpt
                env     <- readIORef envRef
                s       <- readIORef stateRef
                traverse (writeIORef iRef) iOpt
                let evtOpt = PdfSelection <$> uriOpt <*> nameOpt
                (s', _) <- execRWST (traverse_ fPdf evtOpt) env s
                writeIORef stateRef s'
                ahbox <- Gtk.alignmentNew 0 0 1 1
                Gtk.containerAdd ahbox hbox
                Gtk.boxPackStart vbox ahbox Gtk.PackGrow 0
                Gtk.widgetSetSensitive oitem False
                Gtk.widgetShowAll ahbox
    Gtk.on iitem Gtk.menuItemActivate $ do
        resp <- Gtk.dialogRun jdialog
        Gtk.widgetHide jdialog
        case resp of
            Gtk.ResponseCancel -> return ()
            Gtk.ResponseOk -> do
                fOpt    <- Gtk.fileChooserGetFilename jdialog
                env     <- readIORef envRef
                s       <- readIORef stateRef
                (s', _) <- execRWST (traverse_ (jsonLF . JsonLoad) fOpt) env s
                writeIORef stateRef s'
    Gtk.on prev Gtk.buttonActivated $ do
        env     <- readIORef envRef
        s       <- readIORef stateRef
        (s', _) <- execRWST (prevPF PrevPage) env s
        writeIORef stateRef s'
    Gtk.on next Gtk.buttonActivated $ do
        env     <- readIORef envRef
        s       <- readIORef stateRef
        (s', _) <- execRWST (nextPF NextPage) env s
        writeIORef stateRef s'
    Gtk.on minus Gtk.buttonActivated $ do
        env     <- readIORef envRef
        s       <- readIORef stateRef
        (s', _) <- execRWST (minusPF PrevZoom) env s
        writeIORef stateRef s'
    Gtk.on plus Gtk.buttonActivated $ do
        env     <- readIORef envRef
        s       <- readIORef stateRef
        (s', _) <- execRWST (plusPF NextZoom) env s
        writeIORef stateRef s'
    Gtk.on rem Gtk.buttonActivated $ do
        env     <- readIORef envRef
        s       <- readIORef stateRef
        (s', _) <- execRWST (remF (RemoveRect $ error "not now")) env s
        writeIORef stateRef s'
    Gtk.on sel Gtk.treeSelectionSelectionChanged $ do
        env     <- readIORef envRef
        s       <- readIORef stateRef
        sOpt    <- Gtk.treeSelectionGetSelected sel
        (s', _) <- execRWST
                   (traverse_ (selF <=< liftIO . retrieveRect store) sOpt) env s
        writeIORef stateRef s'
    Gtk.on area Gtk.exposeEvent $ Gtk.tryEvent $ liftIO $ do
        env     <- readIORef envRef
        s       <- readIORef stateRef
        (s', _) <- execRWST (drawingF Drawing) env s
        writeIORef stateRef s'
    Gtk.on area Gtk.motionNotifyEvent $ Gtk.tryEvent $ do
        (x',y') <- Gtk.eventCoordinates
        liftIO $ do
            env <- readIORef envRef
            s   <- readIORef stateRef
            v   <- readIORef iRef
            let ratio = getRatio s v
                (x,y) = (x'/ratio, y'/ratio)
                move  = Move x y
            (s', _) <- execRWST (moveF move) env s
            let env1 = env { _enginePrevX = x, _enginePrevY = y }
            writeIORef envRef env1
            writeIORef stateRef s'
    Gtk.on area Gtk.buttonPressEvent $ Gtk.tryEvent $ do
        (x',y') <- Gtk.eventCoordinates
        liftIO $ do
            env <- readIORef envRef
            s   <- readIORef stateRef
            v   <- readIORef iRef
            let ratio = getRatio s v
                (x,y) = (x'/ratio, y'/ratio)
                press = Press x y
            (s', _) <- execRWST (pressF press) env s
            let env1 = env { _enginePrevX = x, _enginePrevY = y }
            writeIORef envRef env1
            writeIORef stateRef s'
    Gtk.on area Gtk.buttonReleaseEvent $ Gtk.tryEvent $ liftIO $ do
        env     <- readIORef envRef
        s       <- readIORef stateRef
        (s', _) <- execRWST (releaseF Release) env s
        writeIORef stateRef s'
    Gtk.on area Gtk.enterNotifyEvent $ Gtk.tryEvent $ liftIO $ do
        env     <- readIORef envRef
        s       <- readIORef stateRef
        (s', _) <- execRWST (enterF Enter) env s
        writeIORef stateRef s'

    Gtk.containerAdd window wvbox
    Gtk.set window windowParams
    Gtk.onDestroy window Gtk.mainQuit
    Gtk.widgetShowAll window
    Gtk.mainGUI
  where
    makeInternal uri = do
        v <- loadPdf uri
        return v
    retrieveRect store it =
        let idx = Gtk.listStoreIterToIndex it in
        fmap RectSelected (Gtk.listStoreGetValue store idx)


createPdfChooserDialog :: Gtk.Window -> IO Gtk.FileChooserDialog
createPdfChooserDialog win = do
  ch   <- Gtk.fileChooserDialogNew title
          (Just win) Gtk.FileChooserActionOpen responses
  filt <- Gtk.fileFilterNew
  Gtk.fileFilterAddPattern filt "*.pdf"
  Gtk.fileFilterSetName filt "PDF File"
  Gtk.fileChooserAddFilter ch filt
  return ch
    where
      responses = [("Open", Gtk.ResponseOk)
                  ,("Cancel", Gtk.ResponseCancel)]
      title = Just "Open a PDF file"

createJsonChooserDialog :: Gtk.Window -> IO Gtk.FileChooserDialog
createJsonChooserDialog win = do
  ch   <- Gtk.fileChooserDialogNew title (Just win)
          Gtk.FileChooserActionSave responses
  filt <- Gtk.fileFilterNew
  Gtk.fileFilterAddPattern filt "*.json"
  Gtk.fileFilterSetName filt "Json File"
  Gtk.fileChooserAddFilter ch filt
  Gtk.fileChooserSetDoOverwriteConfirmation ch True
  return ch
    where
      responses = [("Save", Gtk.ResponseOk)
                  ,("Cancel", Gtk.ResponseCancel)]
      title = Just "Open a Json file"

windowParams :: [Gtk.AttrOp Gtk.Window]
windowParams =
    [Gtk.windowTitle          := "Dhek PDF Viewer"
    ,Gtk.windowDefaultWidth   := 800
    ,Gtk.windowDefaultHeight  := 600
    ,Gtk.containerBorderWidth := 10]

loadPdf :: FilePath -> IO Viewer
loadPdf path = do
  doc   <- fmap fromJust (Poppler.documentNewFromFile path Nothing)
  nb    <- Poppler.documentGetNPages doc
  pages <- loadPages doc
  return (Viewer doc pages 1 nb 100 3 1.0 (boardsNew nb))

loadPages :: Poppler.Document -> IO (Array Int PageItem)
loadPages doc = do
    nb <- Poppler.documentGetNPages doc
    fmap (array (1,nb)) (traverse go [1..nb])
  where
    go i = do
        page  <- Poppler.documentGetPage doc (i-1)
        (w,h) <- Poppler.pageGetSize page
        return (i, PageItem page w h)

getRatio :: EngineState -> Viewer -> Double
getRatio s v = (base * zoom) / width
  where
    pIdx  = _engineCurPage s
    zIdx  = _engineCurZoom s
    pages = _viewerPages v
    base  = 777
    width = pageWidth (pages ! pIdx)
    zoom  = zoomValues ! zIdx

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
