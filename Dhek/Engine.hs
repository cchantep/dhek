{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
module Dhek.Engine where

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
    , (%~))
import Control.Monad ((<=<))
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.RWS.Strict
import Control.Monad.State

import Data.Array (Array, array, (!))
import Data.Foldable (foldMap, traverse_)
import qualified Data.IntMap as I
import Data.IORef
import Data.Maybe (fromJust)
import Data.Traversable (traverse)

import Graphics.UI.Gtk ( AttrOp( (:=) ))
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import qualified Graphics.UI.Gtk.Poppler.Page     as Poppler

import Dhek.Types
import Dhek.Utils (takeFileName)

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

data RectSelected = RectSelected

data Drawing = Drawing
    { drawingArea  :: !Gtk.DrawingArea
    , drawingPage  :: !PageItem
    , drawingRatio :: !Double
    }

data Move = Move !Double !Double
data Press = Press !Double !Double
data Release = Release

data Enter = Enter

data EngineState = EngineState
    { _engineCurPage   :: {-# UNPACK #-} !Int
    , _engineCurZoom   :: {-# UNPACK #-} !Int
    , _engineRectId    :: {-# UNPACK #-} !Int
    , _engineCollision :: !Bool
    , _engineDraw      :: !Bool
    , _engineEvent     :: !(Maybe BoardEvent)
    , _engineSelection :: !(Maybe Rect)
    , _engineSelected  :: !(Maybe Rect)
    , _engineCursor    :: !(Maybe Gtk.CursorType)
    , _engineAddedRect :: !(Maybe Rect)
    , _engineRemRect   :: !(Maybe Rect)
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
        let neg1 :: forall a. Num a => a
            neg1 = negate 1 in
        EngineEnv neg1 neg1 neg1 [] [] Nothing Nothing

    sNew = EngineState
           1
           3
           0
           True
           False
           Nothing
           Nothing
           Nothing
           Nothing
           Nothing
           Nothing

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
    pEntry <- Gtk.entryNew
    pCombo <- Gtk.comboBoxNew
    atable <- Gtk.tableNew 3 3 False
    nlabel <- Gtk.labelNew (Just "Name")
    tlabel <- Gtk.labelNew (Just "Type")
    salign <- Gtk.alignmentNew 0 0 1 0
    ualign <- Gtk.alignmentNew 0.5 0 0 0
    nalign <- Gtk.alignmentNew 0 0.5 0 0
    talign <- Gtk.alignmentNew 0 0.5 0 0
    cstore  <- Gtk.comboBoxSetModelText pCombo
    table  <- Gtk.tableNew 2 2 False
    tvbox  <- Gtk.vBoxNew False 10
    sep    <- Gtk.hSeparatorNew
    Gtk.containerAdd viewport area
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
    col <- Gtk.treeViewColumnNew
    Gtk.treeViewColumnSetTitle col "Areas"
    trenderer <- Gtk.cellRendererTextNew
    Gtk.cellLayoutPackStart col trenderer False
    let mapping r = [Gtk.cellText := r ^. rectName]
    Gtk.cellLayoutSetAttributes col trenderer store mapping
    Gtk.treeViewAppendColumn treeV col
    Gtk.scrolledWindowAddWithViewport tswin treeV
    Gtk.scrolledWindowSetPolicy tswin Gtk.PolicyAutomatic Gtk.PolicyAutomatic
    -- Properties --
    nlabel <- Gtk.labelNew (Just "Name")
    tlabel <- Gtk.labelNew (Just "Type")
    salign <- Gtk.alignmentNew 0 0 1 0
    ualign <- Gtk.alignmentNew 0.5 0 0 0
    nalign <- Gtk.alignmentNew 0 0.5 0 0
    talign <- Gtk.alignmentNew 0 0.5 0 0
    tstore  <- Gtk.comboBoxSetModelText pCombo
    table  <- Gtk.tableNew 2 2 False
    tvbox  <- Gtk.vBoxNew False 10
    sep    <- Gtk.hSeparatorNew
    Gtk.containerAdd nalign nlabel
    Gtk.containerAdd talign tlabel
    Gtk.tableAttachDefaults table nalign 0 1 0 1
    Gtk.tableAttachDefaults table pEntry 1 2 0 1
    Gtk.tableAttachDefaults table talign 0 1 1 2
    Gtk.tableAttachDefaults table pCombo 1 2 1 2
    Gtk.tableSetRowSpacings table 10
    Gtk.tableSetColSpacings table 10
    traverse_ (Gtk.listStoreAppend tstore) ["text", "checkbox"]
    Gtk.containerAdd salign sep
    Gtk.boxPackStart tvbox table Gtk.PackNatural 0
    Gtk.boxPackStart vleft salign Gtk.PackNatural 0
    Gtk.containerAdd vleft tvbox
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

        prepareAll :: forall a t.
                      (Double -> Double -> t)
                   -> EngineCallback t
                   -> Double
                   -> Double
                   -> Gtk.EventM a ()
        prepareAll cs k x' y' = liftIO $ do
            env <- readIORef envRef
            s   <- readIORef stateRef
            v   <- readIORef iRef
            let ratio = getRatio s v
                (x,y) = (x'/ratio, y'/ratio)
                evt   = cs x y
                rects = getRects s v
                oOpt  = getOverRect x y rects
                aOpt  = getOverArea ratio x y =<< oOpt
                env1  = env { _engineOverRect = oOpt
                            , _engineOverArea = aOpt
                            , _engineRects    = rects
                            }

            (s1,_) <- execRWST (k evt) env1 s
            let env2 = env1 { _enginePrevX = x, _enginePrevY = y }
            writeIORef envRef env2
            evalReq v env2 s s1

        evalReq v env s0 s1 = do
            traverse_ update rOpt
            traverse_ onRem rmOpt
            frame  <- Gtk.widgetGetDrawWindow area
            curOpt <- traverse Gtk.cursorNew ctOpt
            Gtk.drawWindowSetCursor frame curOpt
            let s2 = s1 & engineAddedRect .~ Nothing
                        & engineRemRect   .~ Nothing
            when draw $ do
                let s3 = s2 & engineDraw .~ False
                writeIORef stateRef s3
                Gtk.widgetQueueDraw area
            when (not draw) (writeIORef stateRef s2)
          where
            rOpt  = s1 ^. engineAddedRect
            pId   = s1 ^. engineCurPage
            draw  = s1 ^. engineDraw
            ctOpt = s1 ^. engineCursor
            rmOpt = s1 ^. engineRemRect

            onRem r =
                let id = r ^. rectId
                    v1 = v & viewerBoards.boardsMap.at pId.traverse.boardRects.at id .~ Nothing in
                writeIORef iRef v1

            pageMap = v ^. viewerBoards.boardsMap.at pId.traverse.boardRects

            update r = do
                iOpt <- lookupStoreIter (p r) store
                maybe (insert r) (Gtk.treeSelectionSelectIter sel) iOpt
                let id = r ^. rectId
                    v1 = v & viewerBoards.boardsMap.at pId.traverse.boardRects.at id ?~ r
                    env1 = env { _engineRects = getRects s1 v1 }
                writeIORef envRef env1
                writeIORef iRef v1
            insert r = do
                Gtk.listStoreAppend store r
                iOpt <- lookupStoreIter (p r) store
                traverse_ (Gtk.treeSelectionSelectIter sel) iOpt

            p r x = (x ^. rectId) == (r ^. rectId)

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
                let evtOpt = PdfSelection <$> uriOpt <*> nameOpt
                    nb     = v ^. viewerPageCount
                (s', _) <- execRWST (traverse_ fPdf evtOpt) env (initState v s)
                writeIORef stateRef s'
                ahbox <- Gtk.alignmentNew 0 0 1 1
                Gtk.containerAdd ahbox hbox
                Gtk.boxPackStart wvbox ahbox Gtk.PackGrow 0
                Gtk.widgetSetSensitive oitem False
                Gtk.widgetSetSensitive prev False
                Gtk.widgetSetSensitive next (nb /= 1)
                Gtk.windowSetTitle window
                        (name ++ " (page 1 / " ++ show nb ++ ")")
                Gtk.widgetShowAll ahbox
    Gtk.on iitem Gtk.menuItemActivate $ do
        resp <- Gtk.dialogRun jdialog
        Gtk.widgetHide jdialog
        case resp of
            Gtk.ResponseCancel -> return ()
            Gtk.ResponseOk     -> do
                fOpt    <- Gtk.fileChooserGetFilename jdialog
                env     <- readIORef envRef
                s       <- readIORef stateRef
                (s', _) <- execRWST (traverse_ (jsonLF . JsonLoad) fOpt) env s
                writeIORef stateRef s'
    -- Previous Button ---
    Gtk.on prev Gtk.buttonActivated $ do
        env <- readIORef envRef
        v   <- readIORef iRef
        let nb     = _enginePageCount env
            name   = _engineFilename env
            action = do
                i <- engineCurPage <-= 1
                liftIO $ do
                    Gtk.widgetSetSensitive prev (i > 1)
                    Gtk.widgetSetSensitive next True
                    Gtk.windowSetTitle window
                        (name ++ " (page " ++ show i ++ " / " ++ show nb ++ ")")
        s  <- readIORef stateRef
        s' <- execStateT action s
        let env1 = env { _engineRects = getRects s' v}
        writeIORef envRef env1
        writeIORef stateRef s'
        Gtk.widgetQueueDraw area
    --- Next Button ---
    Gtk.on next Gtk.buttonActivated $ do
        env <- readIORef envRef
        v   <- readIORef iRef
        let nb     = _enginePageCount env
            name   = _engineFilename env
            action = do
                i <- engineCurPage <+= 1
                liftIO $ do
                    Gtk.widgetSetSensitive prev True
                    Gtk.widgetSetSensitive next (i < nb)
                    Gtk.windowSetTitle window
                        (name ++ " (page " ++ show i ++ " / " ++ show nb ++ ")")
        s  <- readIORef stateRef
        s' <- execStateT action s
        let env1 = env { _engineRects = getRects s' v}
        writeIORef envRef env1
        writeIORef stateRef s'
        Gtk.widgetQueueDraw area
    --- Minus Button ---
    Gtk.on minus Gtk.buttonActivated $ do
        let action = do
                i <- engineCurZoom <-= 1
                liftIO $ do
                    Gtk.widgetSetSensitive minus (i > 1)
                    Gtk.widgetSetSensitive plus True
        s  <- readIORef stateRef
        s' <- execStateT action s
        writeIORef stateRef s'
        Gtk.widgetQueueDraw area
    --- Plus Button ---
    Gtk.on plus Gtk.buttonActivated $ do
        let action = do
                 i <- engineCurZoom <+= 1
                 liftIO $ do
                     Gtk.widgetSetSensitive minus True
                     Gtk.widgetSetSensitive plus (i < 10)
        s  <- readIORef stateRef
        s' <- execStateT action s
        writeIORef stateRef s'
        Gtk.widgetQueueDraw area
    Gtk.on rem Gtk.buttonActivated $ do
        env     <- readIORef envRef
        s       <- readIORef stateRef
        (s', _) <- execRWST (remF (RemoveRect $ error "not now")) env s
        writeIORef stateRef s'
    --- Selection ---
    Gtk.on sel Gtk.treeSelectionSelectionChanged $ do
        env  <- readIORef envRef
        s    <- readIORef stateRef
        sOpt <- Gtk.treeSelectionGetSelected sel
        let callback it = do
                let idx = Gtk.listStoreIterToIndex it
                r <- Gtk.listStoreGetValue store idx
                let action = do
                        engineSelected ?= r
                        selF RectSelected
                        engineDraw .= False
                (s',_) <- execRWST action env s
                writeIORef stateRef s'
                Gtk.widgetQueueDraw area
        traverse_ callback sOpt
    Gtk.on area Gtk.exposeEvent $ Gtk.tryEvent $ liftIO $ do
        env     <- readIORef envRef
        s       <- readIORef stateRef
        v       <- readIORef iRef
        let ratio = getRatio s v
            page  = getPage s v
        (s', _) <- execRWST (drawingF $ Drawing area page ratio) env s
        writeIORef stateRef s'
    Gtk.on area Gtk.motionNotifyEvent $ Gtk.tryEvent $ do
        (x',y') <- Gtk.eventCoordinates
        prepareAll Move moveF x' y'
    Gtk.on area Gtk.buttonPressEvent $ Gtk.tryEvent $ do
        (x',y') <- Gtk.eventCoordinates
        prepareAll Press pressF x' y'
    Gtk.on area Gtk.buttonReleaseEvent $ Gtk.tryEvent $ liftIO $ do
        env    <- readIORef envRef
        s      <- readIORef stateRef
        v      <- readIORef iRef
        (s',_) <- execRWST (releaseF Release) env s
        evalReq v env s s'
    Gtk.on area Gtk.enterNotifyEvent $ Gtk.tryEvent $ liftIO $ do
        env     <- readIORef envRef
        s       <- readIORef stateRef
        (s', _) <- execRWST (enterF Enter) env s
        writeIORef stateRef s'
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
    Gtk.containerAdd window wvbox
    Gtk.set window windowParams
    Gtk.onDestroy window Gtk.mainQuit
    Gtk.widgetShowAll window
    Gtk.mainGUI
  where
    makeInternal uri = do
        v <- loadPdf uri
        return v

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

freshId :: RWST EngineEnv () EngineState IO Int
freshId = engineRectId <+= 1

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
    let overed r
            | isOver 1.0 x y r = Just r
            | otherwise        = Nothing

        (First oOpt) = foldMap (First . overed) rs in oOpt

getOverArea :: Double -> Double -> Double -> Rect -> Maybe Area
getOverArea ratio x y r =
    let overed ratio a
            | isOver 1.0 x y (rectArea (5/ratio) r a) = Just a
            | otherwise                               = Nothing

        (First aOpt) = foldMap (First . overed ratio) (enumFrom TOP_LEFT) in
    aOpt

getRects :: EngineState -> Viewer -> [Rect]
getRects s v =
    let pId   = s ^. engineCurPage
        rects =
            v ^. viewerBoards.boardsMap.at pId.traverse.boardRects.to I.elems in
    rects

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
                False
                (s ^. engineCollision)
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
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
