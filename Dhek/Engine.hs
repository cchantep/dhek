{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Engine
--
--
--------------------------------------------------------------------------------
module Dhek.Engine where

--------------------------------------------------------------------------------
import           Prelude hiding (foldr)
import           Control.Monad (when)
import           Control.Monad.State
import           Data.Array (Array, array, (!))
import           Data.Foldable (find, foldr, for_, traverse_)
import qualified Data.IntMap as I
import           Data.IORef
import           Data.Maybe (fromJust)
import           Data.Traversable (for)

--------------------------------------------------------------------------------
import           Control.Lens
import qualified Graphics.UI.Gtk                  as Gtk
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import qualified Graphics.UI.Gtk.Poppler.Page     as Poppler

--------------------------------------------------------------------------------
import Dhek.GUI
import Dhek.Instr
import Dhek.Types
import Dhek.Utils (takeFileName, trimString)

--------------------------------------------------------------------------------
data Interpreter =
    Interpreter
    { _internal :: IORef Viewer
    , _state    :: IORef EngineState
    , _env      :: IORef EngineEnv
    , _gui      :: GUI
    }

--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
data EngineEnv = EngineEnv
    { _enginePrevX     :: {-# UNPACK #-} !Double
    , _enginePrevY     :: {-# UNPACK #-} !Double
    , _enginePageCount :: {-# UNPACK #-} !Int
    , _engineFilename  :: !String
    , _engineRects     :: ![Rect]
    , _engineOverRect  :: !(Maybe Rect)
    , _engineOverArea  :: !(Maybe Area)
    }

--------------------------------------------------------------------------------
makeLenses ''EngineState

--------------------------------------------------------------------------------
makeInterpreter :: GUI -> IO Interpreter
makeInterpreter gui = do
    eRef <- newIORef envNew
    sRef <- newIORef stateNew
    vRef <- newIORef undefined
    return Interpreter{ _internal = vRef
                      , _state = sRef
                      , _env = eRef
                      , _gui = gui
                      }

--------------------------------------------------------------------------------
envNew :: EngineEnv
envNew =
    let neg1 :: forall a. Num a => a
        neg1 = negate 1 in
    EngineEnv{ _enginePrevX = neg1
             , _enginePrevY = neg1
             , _enginePageCount = neg1
             , _engineFilename = ""
             , _engineRects = []
             , _engineOverRect = Nothing
             , _engineOverArea = Nothing
             }

--------------------------------------------------------------------------------
stateNew :: EngineState
stateNew =
    EngineState{ _engineCurPage = 1
               , _engineCurZoom = 3
               , _engineRectId = 0
               , _engineOverlap = False
               , _engineDraw = False
               , _enginePropLabel =  ""
               , _enginePropType = Nothing
               , _engineEvent = Nothing
               , _engineSelection = Nothing
               , _engineSelected = Nothing
               , _engineCursor = Nothing
               , _engineAddedRect = Nothing
               , _engineRemRect = Nothing
               , _enginePrevPos = (negate 1, negate 1)
               , _engineColPos = Nothing
               }

--------------------------------------------------------------------------------
runProgram :: Interpreter -> DhekProgram () -> IO ()
runProgram i = runProgramWithCoord i (x, x) where x = (-1)

--------------------------------------------------------------------------------
runProgramWithCoord :: Interpreter
                    -> (Double, Double)
                    -> DhekProgram ()
                    -> IO ()
runProgramWithCoord i (x', y') p = do
    s     <- readIORef $ _state i
    v     <- readIORef $ _internal i
    env   <- readIORef $ _env i
    frame <- Gtk.widgetGetDrawWindow $ guiDrawingArea $ _gui i
    let gui   = _gui i
        ratio = _getRatio s v
        x     = x' / ratio
        y     = y' / ratio
        rects = _getRects s v
        pId   = s ^. engineCurPage
        nb    = v ^. viewerPageCount
        page  = _getPage s v
        oOpt  = _getOverRect x y rects
        aOpt  = _getOverArea ratio x y =<< oOpt
        filename = _engineFilename env
        pred r x = (x ^. rectId) == (r ^. rectId)

        susp (GetPointer k)  s v = k (x,y) s v
        susp (GetOverRect k) s v = k oOpt s v
        susp (GetOverArea k) s v = k aOpt s v
        susp (GetSelected k) s v = k (s ^. engineSelected) s v
        susp (SetSelected rOpt k) s v =
            case rOpt of
                Just r -> do
                    let id = r ^. rectId
                        s1 = s & engineSelected ?~ r
                        v1 = v & viewerBoards.boardsMap.at pId.traverse.boardRects.at id ?~ r
                    iOpt <- lookupStoreIter (pred r) $ guiRectStore gui
                    for_ iOpt $ \it -> do
                        let idx = Gtk.listStoreIterToIndex it
                        Gtk.listStoreSetValue (guiRectStore gui) idx r
                        Gtk.treeSelectionSelectIter (guiRectTreeSelection gui) it
                    Gtk.entrySetText (guiNameEntry gui) (r ^. rectName)
                    case r ^. rectValue of
                        Nothing -> Gtk.entrySetText (guiValueEntry gui) ""
                        Just v  -> Gtk.entrySetText (guiValueEntry gui) v
                    tOpt <- lookupStoreIter (\x -> x == (r ^. rectType)) (guiTypeStore gui)
                    writeIORef (_state i) s1 -- combobox hack in order to prevent sync issue
                    traverse_ (Gtk.comboBoxSetActiveIter $ guiTypeCombo gui) tOpt
                    Gtk.widgetSetSensitive (guiRemoveButton gui) True
                    Gtk.widgetSetSensitive (guiApplyButton gui) True
                    Gtk.widgetSetSensitive (guiTypeCombo gui) True
                    Gtk.widgetSetSensitive (guiNameEntry gui) True
                    k s1 v1
                Nothing -> susp (UnselectRect k) s v
        susp (GetEvent k) s v   = k (s ^. engineEvent) s v
        susp (SetEvent e k) s v =
            let s1 = s & engineEvent .~ e in
            k s1 v
        susp (GetRects k) s v = k rects s v
        susp (GetRatio k) s v = k ratio s v
        susp (GetSelection k) s v = k (s ^. engineSelection) s v
        susp (SetSelection r k) s v =
            let s1 = s & engineSelection .~ r in
            k s1 v
        susp (GetPage k) s v = k page s v
        susp (GetCurPage k) s v = k (s ^. engineCurPage) s v
        susp (GetPageCount k ) s v = k nb s v
        susp (SetCursor tOpt k) s v = do
            cOpt <- traverse Gtk.cursorNew tOpt
            Gtk.drawWindowSetCursor frame cOpt
            k s v
        susp (FreshId k) s v =
            let (id, s1) = s & engineRectId <+~ 1 in
            k id s1 v
        susp (IncrPage k) s v = do
            let (ncur, s1) = s & engineSelected .~ Nothing
                               & engineCurPage <+~ 1
                rects2     = _getRects s1 v
            Gtk.widgetSetSensitive (guiPrevButton gui) True
            Gtk.widgetSetSensitive (guiNextButton gui) (ncur < nb)
            Gtk.listStoreClear $ guiRectStore gui
            traverse_ (Gtk.listStoreAppend $ guiRectStore gui) rects2
            k s1 v
        susp (DecrPage k) s v = do
            let (ncur, s1) = s & engineSelected .~ Nothing
                               & engineCurPage <-~ 1
                rects2     = _getRects s1 v
            Gtk.widgetSetSensitive (guiPrevButton gui) (ncur > 1)
            Gtk.widgetSetSensitive (guiNextButton gui) True
            Gtk.listStoreClear (guiRectStore gui)
            traverse_ (Gtk.listStoreAppend $ guiRectStore gui) rects2
            k s1 v
        susp (IncrZoom k) s v = do
            let (ncur, s1) = s & engineCurZoom <+~ 1
            Gtk.widgetSetSensitive (guiZoomOutButton gui) True
            Gtk.widgetSetSensitive (guiZoomInButton gui) (ncur < 10)
            k s1 v
        susp (DecrZoom k) s v = do
            let (ncur, s1) = s & engineCurZoom <-~ 1
            Gtk.widgetSetSensitive (guiZoomOutButton gui) (ncur > 1)
            Gtk.widgetSetSensitive (guiZoomInButton gui) True
            k s1 v
        susp (RemoveRect r k) s v = do
            let id = r ^. rectId
                v1 = v & viewerBoards.boardsMap.at pId.traverse.boardRects.at id .~ Nothing
            iOpt <- lookupStoreIter (pred r) (guiRectStore gui)
            for_ iOpt $ \it ->
                let idx = Gtk.listStoreIterToIndex it in
                Gtk.listStoreRemove (guiRectStore gui) idx
            k s v1
        susp (DetachRect r k) s v = do
            let id = r ^. rectId
                v1 = v & viewerBoards.boardsMap.at pId.traverse.boardRects.at id .~ Nothing
            k s v1
        susp (AttachRect r k) s v = do
            let id = r ^. rectId
                v1 = v & viewerBoards.boardsMap.at pId.traverse.boardRects.at id ?~ r
            k s v1
        susp (AddRect r k) s v = do
            let id = r ^. rectId
                v1 = v & viewerBoards.boardsMap.at pId.traverse.boardRects.at id ?~ r
            Gtk.listStoreAppend (guiRectStore gui) r
            iOpt <- lookupStoreIter (pred r) (guiRectStore gui)
            traverse_ (Gtk.treeSelectionSelectIter $ guiRectTreeSelection gui) iOpt
            k s v1
        susp (GetEntryText e k) s v =
            case e of
                PropEntry -> do
                    r <- lookupEntryText $ guiNameEntry gui
                    k r s v
                ValueEntry -> do
                    r <- lookupEntryText $ guiValueEntry gui
                    k r s v
        susp (GetComboText e k) s v =
            case e of
                PropCombo -> do
                    tOpt <- Gtk.comboBoxGetActiveText $ guiTypeCombo gui
                    k tOpt s v
        susp (UnselectRect k) s v = do
            let s1 = s & engineSelected .~ Nothing
            Gtk.widgetSetSensitive (guiTypeCombo gui) False
            Gtk.widgetSetSensitive (guiNameEntry gui) False
            Gtk.widgetSetSensitive (guiRemoveButton gui) False
            Gtk.widgetSetSensitive (guiApplyButton gui) False
            Gtk.entrySetText (guiNameEntry gui) ""
            Gtk.comboBoxSetActive (guiTypeCombo gui) (negate 1)
            k s1 v
        susp (Draw k) s v =
            let s1 = s & engineDraw .~ True in
            k s1 v
        susp (SetTitle t k) s v = do
            Gtk.windowSetTitle (guiWindow gui) t
            k s v
        susp (GetFilename k) s v = k filename s v
        susp (GetFrameSize k) s v = do
            size <- Gtk.drawableGetSize frame
            k size s v
        susp (ExecCairo r k) s v = do
            Gtk.renderWithDrawable frame r
            k s v
        susp (SizeRequest rx ry k) s v = do
            Gtk.widgetSetSizeRequest (guiDrawingArea gui) rx ry
            k s v
        susp (ShowError e k) s v = do
            m <- Gtk.messageDialogNew (Just $ guiWindow gui)
                 [Gtk.DialogModal] Gtk.MessageError Gtk.ButtonsOk e
            Gtk.dialogRun m
            Gtk.widgetHide m
            k s v
        susp (PerformIO action k) s v = do
            b <- action
            k b s v
        susp (GetTreeSelection k) s v = do
            iOpt <- Gtk.treeSelectionGetSelected $ guiRectTreeSelection gui
            rOpt <- for iOpt $ \it ->
                let idx = Gtk.listStoreIterToIndex it in
                Gtk.listStoreGetValue (guiRectStore gui) idx
            k rOpt s v
        susp (NewGuide t k) s v =
            let v1 = v & viewerBoards.boardsCurGuide ?~ Guide 0 t in
            k s v1
        susp (UpdateGuide k) s v = do
            x <- Gtk.get (guiHRuler gui) Gtk.rulerPosition
            y <- Gtk.get (guiVRuler gui) Gtk.rulerPosition
            let upd g =
                    let v = case g ^. guideType of
                            GuideVertical   -> x
                            GuideHorizontal -> y in
                    g & guideValue .~ v
                v1 = v & viewerBoards.boardsCurGuide %~ fmap upd
            k s v1
        susp (AddGuide k) s v =
            let action = do
                    gOpt <- use $ viewerBoards.boardsCurGuide
                    gs   <- use $ viewerBoards.boardsGuides
                    let gs1 = foldr (:) gs gOpt
                    viewerBoards.boardsCurGuide .= Nothing
                    viewerBoards.boardsGuides   .= gs1
                v1 = execState action v in
            k s v1
        susp (GetCurGuide k) s v =  k (v ^. viewerBoards.boardsCurGuide) s v
        susp (GetGuides k) s v = k (v ^. viewerBoards.boardsGuides) s v
        susp (SelectJsonFile k) s v = do
            resp <- Gtk.dialogRun $ guiJsonSaveDialog gui
            Gtk.widgetHide $ guiJsonSaveDialog gui
            case resp of
                Gtk.ResponseOk -> do
                    fOpt <- Gtk.fileChooserGetFilename $ guiJsonSaveDialog gui
                    k fOpt s v
                _ -> k Nothing s v
        susp (GetAllRects k) s v =
            let tup (i, b) = (i, b ^. boardRects.to I.elems)
                list       = fmap tup . I.toList in
            k (v ^. viewerBoards.boardsMap.to list) s v
        susp (OpenJsonFile k) s v = do
            resp <- Gtk.dialogRun $ guiJsonOpenDialog gui
            Gtk.widgetHide $ guiJsonOpenDialog gui
            case resp of
                Gtk.ResponseOk -> do
                    fOpt <- Gtk.fileChooserGetFilename $ guiJsonOpenDialog gui
                    k fOpt s v
                _ -> k Nothing s v
        susp (SetRects xs k) s v = do
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
                rects2        = _getRects s1 v1
            Gtk.listStoreClear $ guiRectStore gui
            traverse_ (Gtk.listStoreAppend $ guiRectStore gui) rects2
            k s1 v1
        susp (Active o b k) s v =
            case o of
                Overlap -> do
                    let s1 = s & engineOverlap .~ b
                    Gtk.checkMenuItemSetActive (guiOverlapMenuItem gui) b
                    k s1 v
        susp (IsActive o k) s v =
            case o of
                Overlap -> k (s ^. engineOverlap) s v
        susp (PrevPointer k) s v =
            k (s ^. enginePrevPos) s v
        susp (SetCol o k) s v =
            let s1 = s & engineColPos .~ o in
            k s1 v
        susp (GetCol k) s v =
            k (s ^. engineColPos) s v
        susp (SetValuePropVisible b k) s v = do
            if b
                then do
                    Gtk.widgetSetChildVisible (guiValueEntryAlign gui) True
                    Gtk.widgetSetChildVisible (guiValueEntry gui) True
                    Gtk.widgetShowAll $ guiValueEntryAlign gui
                    Gtk.widgetShowAll $ guiValueEntry gui
                else do
                   Gtk.widgetHideAll $ guiValueEntryAlign gui
                   Gtk.widgetHideAll $ guiValueEntry gui
            k s v
        susp (IsToggleActive t k) s v =
            case t of
                DrawToggle ->
                    Gtk.toggleButtonGetActive (guiDrawToggle gui) >>= \r -> k r s v
                MultiSelToggle ->
                    Gtk.toggleButtonGetActive (guiMultiSelToggle gui) >>= \r -> k r s v
        susp (SetToggleActive t b k) s v = do
            case t of
                DrawToggle -> do
                    Gtk.toggleButtonSetActive (guiDrawToggle gui) b
                MultiSelToggle ->
                    Gtk.toggleButtonSetActive (guiMultiSelToggle gui) b
            k s v

        end a s v = do
            let drawing = s ^. engineDraw
                s1      = s & engineDraw    .~ False
                            & enginePrevPos .~ (x,y)
            writeIORef (_internal i) v
            writeIORef (_state i) s1
            when drawing (Gtk.widgetQueueDraw $ guiDrawingArea gui)

    (foldFree end susp p) s v

--------------------------------------------------------------------------------
lookupStoreIter :: (a -> Bool) -> Gtk.ListStore a -> IO (Maybe Gtk.TreeIter)
lookupStoreIter pred store = Gtk.treeModelGetIterFirst store >>= go
  where
    go (Just it) = do
        a <- Gtk.listStoreGetValue store (Gtk.listStoreIterToIndex it)
        if pred a
            then return (Just it)
            else Gtk.treeModelIterNext store it >>= go
    go _ = return Nothing

--------------------------------------------------------------------------------
lookupEntryText :: Gtk.Entry -> IO (Maybe String)
lookupEntryText entry = do
    txt <- Gtk.entryGetText entry
    let txt1 = trimString txt
        r    = if null txt1 then Nothing else Just txt1
    return r

--------------------------------------------------------------------------------
_getRatio :: EngineState -> Viewer -> Double
_getRatio s v = (base * zoom) / width
  where
    pIdx  = _engineCurPage s
    zIdx  = _engineCurZoom s
    pages = _viewerPages v
    base  = 777
    width = pageWidth (pages ! pIdx)
    zoom  = zoomValues ! zIdx

--------------------------------------------------------------------------------
_getPage :: EngineState -> Viewer -> PageItem
_getPage s v = pages ! pIdx
  where
    pIdx  = _engineCurPage s
    pages = _viewerPages v

--------------------------------------------------------------------------------
_getOverRect :: Double -> Double -> [Rect] -> Maybe Rect
_getOverRect x y rs =
    let overed = isOver 1.0 x y
        oOpt   = find overed rs in
    oOpt

--------------------------------------------------------------------------------
_getOverArea :: Double -> Double -> Double -> Rect -> Maybe Area
_getOverArea ratio x y r =
    let overed a =
            isOver 1.0 x y (rectArea (5/ratio) r a)

        aOpt = find overed (enumFrom TOP_LEFT) in
    aOpt

--------------------------------------------------------------------------------
_getRects :: EngineState -> Viewer -> [Rect]
_getRects s v =
    let pId   = s ^. engineCurPage
        rects =
            v ^. viewerBoards.boardsMap.at pId.traverse.boardRects.to I.elems in
    rects

--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
loadPdf :: Interpreter -> FilePath -> IO ()
loadPdf i path = do
    v <- _loadPdf path
    s <- readIORef $ _state i
    let env  = envNew { _enginePageCount = v ^. viewerPageCount
                      , _engineFilename  = takeFileName path
                      }
        name = _engineFilename env
        nb   = v ^. viewerPageCount
        s'   = stateNew { _engineOverlap = s ^. engineOverlap }
        gui  = _gui i
    writeIORef (_internal i) v
    writeIORef (_env i) env
    writeIORef (_state i) s'
    ahbox <- Gtk.alignmentNew 0 0 1 1
    Gtk.containerAdd ahbox (guiWindowHBox gui)
    Gtk.boxPackStart (guiWindowVBox gui) ahbox Gtk.PackGrow 0
    Gtk.widgetSetSensitive (guiPdfOpenMenuItem gui) False
    Gtk.widgetSetSensitive (guiJsonOpenMenuItem gui) True
    Gtk.widgetSetSensitive (guiJsonSaveMenuItem gui) True
    Gtk.widgetSetSensitive (guiOverlapMenuItem gui) True
    Gtk.widgetSetSensitive (guiPrevButton gui) False
    Gtk.widgetSetSensitive (guiNextButton gui) (nb /= 1)
    Gtk.windowSetTitle (guiWindow gui)
        (name ++ " (page 1 / " ++ show nb ++ ")")
    Gtk.widgetShowAll ahbox

--------------------------------------------------------------------------------
_loadPdf :: FilePath -> IO Viewer
_loadPdf path = do
    doc   <- fmap fromJust (Poppler.documentNewFromFile path Nothing)
    nb    <- Poppler.documentGetNPages doc
    pages <- _loadPages doc
    return (Viewer doc pages 1 nb 100 3 1.0 (boardsNew nb))

--------------------------------------------------------------------------------
_loadPages :: Poppler.Document -> IO (Array Int PageItem)
_loadPages doc = do
    nb <- Poppler.documentGetNPages doc
    fmap (array (1,nb)) (traverse go [1..nb])
  where
    go i = do
        page  <- Poppler.documentGetPage doc (i-1)
        (w,h) <- Poppler.pageGetSize page
        return (i, PageItem page w h)
