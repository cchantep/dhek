{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Engine.Interpreter
--
-- Engine Intruction interpreter
--------------------------------------------------------------------------------
module Dhek.Engine.Interpreter where

--------------------------------------------------------------------------------
import           Prelude hiding (foldr)
import           Control.Monad (when)
import           Data.Array (Array, array, (!))
import           Data.Char (isSpace)
import           Data.Foldable (find, foldr, for_, traverse_)
import qualified Data.IntMap as I
import           Data.IORef
import           Data.List (dropWhileEnd)
import           Data.Maybe (fromJust)
import           Data.Traversable (for)

--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.RWS
import qualified Graphics.UI.Gtk                  as Gtk
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import qualified Graphics.UI.Gtk.Poppler.Page     as Poppler
import           System.FilePath (takeFileName)

--------------------------------------------------------------------------------
import Dhek.Engine.Type
import Dhek.Free
import Dhek.GUI
import Dhek.GUI.Action
import Dhek.Instr
import Dhek.Mode.Duplicate
import Dhek.Mode.Normal
import Dhek.Types

--------------------------------------------------------------------------------
data Interpreter =
    Interpreter
    { _internal :: IORef Viewer
    , _state    :: IORef EngineState
    , _env      :: IORef EngineEnv
    , _gui      :: GUI
    }

--------------------------------------------------------------------------------
drawInterpret :: (DrawEnv -> M a) -> Interpreter -> Pos -> IO ()
drawInterpret k i (x,y) = do
    s <- readIORef $ _state i
    v <- readIORef $ _internal i
    e <- readIORef $ _env i

    ratio <- engineRatio i

    let gui   = _gui i
        pages = v ^. viewerPages
        pid   = s ^. engineCurPage
        mode  = s ^. engineMode
        opts  = DrawEnv{ drawOverlap = s ^. engineOverlap
                       , drawPointer = (x/ratio, y/ratio)
                       , drawRects   = getRects s
                       , drawRatio   = ratio
                       }

    s2 <- runMode mode s (k opts)

    writeIORef (_state i) s2
    liftIO $ Gtk.widgetQueueDraw $ guiDrawingArea gui

--------------------------------------------------------------------------------
_selectRect :: (MonadState EngineState m, MonadIO m) => GUI -> Rect -> m ()
_selectRect gui r = do
     let rid = r ^. rectId

     pid <- use engineCurPage

     engineDrawState.drawSelected ?= r
     engineBoards.boardsMap.at pid.traverse.boardRects.at rid ?= r

     liftIO $ gtkSelectRect r gui

--------------------------------------------------------------------------------
engineCurrentState :: Interpreter -> IO EngineState
engineCurrentState  = readIORef . _state

--------------------------------------------------------------------------------
engineCurrentPage :: Interpreter -> IO PageItem
engineCurrentPage  i = do
    v <- readIORef $ _internal i
    s <- readIORef $ _state i

    let pages = v ^. viewerPages
        pid   = s ^. engineCurPage
        page  = pages ! pid

    return page

--------------------------------------------------------------------------------
engineDrawingArea :: Interpreter -> Gtk.DrawingArea
engineDrawingArea = guiDrawingArea . _gui

--------------------------------------------------------------------------------
engineSetMode :: Mode -> Interpreter -> IO ()
engineSetMode m i = do
    modifyIORef (_state i) (\s -> s { _engineMode = m })

--------------------------------------------------------------------------------
engineRatio :: Interpreter -> IO Double
engineRatio i = do
    v <- readIORef $ _internal i
    s <- readIORef $ _state i
    let pages = v ^. viewerPages
        zoom  = zoomValues ! (s ^. engineCurZoom)
        pid   = s ^. engineCurPage
        width = pageWidth (pages ! pid)
        base  = fromIntegral (v ^. viewerBaseWidth)
        ratio = (base * zoom) / width

    return ratio

--------------------------------------------------------------------------------
makeInterpreter :: GUI -> IO Interpreter
makeInterpreter gui = do
    eRef <- newIORef envNew
    sRef <- newIORef $ stateNew gui
    vRef <- newIORef undefined
    return Interpreter{ _internal = vRef
                      , _state = sRef
                      , _env = eRef
                      , _gui = gui
                      }

--------------------------------------------------------------------------------
envNew :: EngineEnv
envNew =
    EngineEnv{ _enginePrevX = (-1)
             , _enginePrevY = (-1)
             , _enginePageCount = (-1)
             , _engineFilename = ""
             , _engineRects = []
             , _engineOverRect = Nothing
             , _engineOverArea = Nothing
             }

--------------------------------------------------------------------------------
stateNew :: GUI -> EngineState
stateNew gui =
    EngineState{ _engineCurPage = 1
               , _engineCurZoom = 3
               , _engineRectId = 0
               , _engineOverlap = False
               , _engineDraw = False
               , _enginePropLabel =  ""
               , _enginePropType = Nothing
               , _enginePrevPos = (negate 1, negate 1)
               , _engineDrawState = drawStateNew
               , _engineBoards = boardsNew 1
               , _engineMode = normalMode gui
               }

--------------------------------------------------------------------------------
runProgram :: Interpreter -> DhekProgram a -> IO a
runProgram i p = do
    s     <- readIORef $ _state i
    v     <- readIORef $ _internal i
    env   <- readIORef $ _env i
    frame <- Gtk.widgetGetDrawWindow $ guiDrawingArea $ _gui i
    let gui   = _gui i
        ratio = _getRatio s v
        rects = getRects s
        pId   = s ^. engineCurPage
        nb    = v ^. viewerPageCount
        page  = _getPage s v
        filename = _engineFilename env

        susp (GetCurPage k) = (use engineCurPage) >>= k
        susp (GetPageCount k) = k (v ^. viewerPageCount)
        susp (GetSelected k) = (use $ engineDrawState.drawSelected) >>= k
        susp (SetSelected r k) = do
            maybe (liftIO $ gtkUnselect gui) (_selectRect gui) r
            k
        susp (UnselectRect k) = do
            engineDrawState.drawSelected .= Nothing
            liftIO $ gtkUnselect gui
            k
        susp (GetRectangles k) = k rects
        susp (IncrPage k) = do
            engineDrawState.drawSelected .= Nothing
            ncur <- engineCurPage <+= 1
            s    <- get

            let rects2 = getRects s

            liftIO $ do
                Gtk.widgetSetSensitive (guiPrevButton gui) True
                Gtk.widgetSetSensitive (guiNextButton gui) (ncur < nb)
                Gtk.listStoreClear $ guiRectStore gui
                traverse_ (Gtk.listStoreAppend $ guiRectStore gui) rects2
            k
        susp (DecrPage k) = do
            engineDrawState.drawSelected .= Nothing
            ncur <- engineCurPage <-= 1
            s    <- get

            let rects2 = getRects s

            liftIO $ do
                Gtk.widgetSetSensitive (guiPrevButton gui) (ncur > 1)
                Gtk.widgetSetSensitive (guiNextButton gui) True
                Gtk.listStoreClear (guiRectStore gui)
                traverse_ (Gtk.listStoreAppend $ guiRectStore gui) rects2
            k
        susp (IncrZoom k) = do
            ncur <- engineCurZoom <+= 1

            liftIO $ do
                Gtk.widgetSetSensitive (guiZoomOutButton gui) True
                Gtk.widgetSetSensitive (guiZoomInButton gui) (ncur < 10)
            k
        susp (DecrZoom k) = do
            ncur <- engineCurZoom <-= 1

            liftIO $ do
                Gtk.widgetSetSensitive (guiZoomOutButton gui) (ncur > 1)
                Gtk.widgetSetSensitive (guiZoomInButton gui) True
            k
        susp (RemoveRect r k) = do
            let id = r ^. rectId

            engineBoards.boardsMap.at pId.traverse.boardRects.at id .= Nothing

            liftIO $ do
                iOpt <- lookupStoreIter (sameRectId r) (guiRectStore gui)
                for_ iOpt $ \it ->
                    let idx = Gtk.listStoreIterToIndex it in
                    Gtk.listStoreRemove (guiRectStore gui) idx
            k
        susp (GetEntryText e k) =
            case e of
                PropEntry -> do
                    r <- liftIO $ lookupEntryText $ guiNameEntry gui
                    k r
                ValueEntry -> do
                    r <- liftIO $ lookupEntryText $ guiValueEntry gui
                    k r
        susp (GetComboText e k) =
            case e of
                PropCombo -> do
                    tOpt <- liftIO $ Gtk.comboBoxGetActiveText $ guiTypeCombo gui
                    k tOpt
        susp (Draw k) = do
            engineDraw .= True
            k
        susp (SetTitle t k) = do
            liftIO $ Gtk.windowSetTitle (guiWindow gui) t
            k
        susp (GetFilename k) = k filename
        susp (ShowError e k) = do
            liftIO $ do
                m <- Gtk.messageDialogNew (Just $ guiWindow gui)
                     [Gtk.DialogModal] Gtk.MessageError Gtk.ButtonsOk e
                Gtk.dialogRun m
                Gtk.widgetHide m
            k
        susp (PerformIO action k) = (liftIO action) >>= k
        susp (GetTreeSelection k) = do
            rOpt <- liftIO $ do
                iOpt <- Gtk.treeSelectionGetSelected $ guiRectTreeSelection gui
                for iOpt $ \it ->
                    let idx = Gtk.listStoreIterToIndex it in
                    Gtk.listStoreGetValue (guiRectStore gui) idx
            k rOpt
        susp (NewGuide t k) = do
            engineBoards.boardsCurGuide ?= Guide 0 t
            k
        susp (UpdateGuide k) = do
            x <- liftIO $ Gtk.get (guiHRuler gui) Gtk.rulerPosition
            y <- liftIO $ Gtk.get (guiVRuler gui) Gtk.rulerPosition

            let upd g =
                    let v = case g ^. guideType of
                            GuideVertical   -> x
                            GuideHorizontal -> y in
                    g & guideValue .~ v

            engineBoards.boardsCurGuide %= fmap upd
            k
        susp (AddGuide k) = do
            gOpt <- use $ engineBoards.boardsCurGuide
            gs   <- use $ engineBoards.boardsGuides

            let gs1 = foldr (:) gs gOpt

            engineBoards.boardsCurGuide .= Nothing
            engineBoards.boardsGuides   .= gs1

            k
        susp (GetCurGuide k) =  (use $ engineBoards.boardsCurGuide) >>= k
        susp (GetGuides k) = (use $ engineBoards.boardsGuides) >>= k
        susp (SelectJsonFile k) = do
            r <- liftIO $ do
                resp <- Gtk.dialogRun $ guiJsonSaveDialog gui
                Gtk.widgetHide $ guiJsonSaveDialog gui
                case resp of
                    Gtk.ResponseOk ->
                        Gtk.fileChooserGetFilename $ guiJsonSaveDialog gui
                    _ -> return Nothing
            k r
        susp (GetAllRects k) = do
            let tup (i, b) = (i, b ^. boardRects.to I.elems)
                list       = fmap tup . I.toList
            (use $ engineBoards.boardsMap.to list) >>= k
        susp (OpenJsonFile k) = do
            r <- liftIO $ do
                resp <- Gtk.dialogRun $ guiJsonOpenDialog gui
                Gtk.widgetHide $ guiJsonOpenDialog gui
                case resp of
                    Gtk.ResponseOk ->
                        Gtk.fileChooserGetFilename $ guiJsonOpenDialog gui
                    _ -> return Nothing
            k r
        susp (SetRects xs k) = do
            let onEach page r = do
                    id <- boardsState <+= 1
                    let r1 = r & rectId .~ id
                    boardsMap.at page.traverse.boardRects.at id ?= r1

                go (page, rs) = traverse_ (onEach page) rs
                action        = traverse_ go xs
                nb            = length xs
                b             = execState action (boardsNew nb)

            engineBoards                .= b
            engineDrawState.drawFreshId .= b ^. boardsState
            s <- get

            liftIO $ do
                Gtk.listStoreClear $ guiRectStore gui
                traverse_ (Gtk.listStoreAppend $ guiRectStore gui) (getRects s)
            k
        susp (Active o b k) =
            case o of
                Overlap -> do
                    engineOverlap .= b
                    liftIO $ Gtk.checkMenuItemSetActive (guiOverlapMenuItem gui) b
                    k
        susp (IsActive o k) =
            case o of
                Overlap -> (use engineOverlap) >>= k
        susp (SetValuePropVisible b k) = do
            if b
                then liftIO $ do
                    Gtk.widgetSetChildVisible (guiValueEntryAlign gui) True
                    Gtk.widgetSetChildVisible (guiValueEntry gui) True
                    Gtk.widgetShowAll $ guiValueEntryAlign gui
                    Gtk.widgetShowAll $ guiValueEntry gui
                else liftIO $ do
                   Gtk.widgetHideAll $ guiValueEntryAlign gui
                   Gtk.widgetHideAll $ guiValueEntry gui
            k
        susp (IsToggleActive t k) =
            case t of
                DrawToggle -> do
                    engineMode .= normalMode gui
                    (liftIO $ Gtk.toggleButtonGetActive (guiDrawToggle gui)) >>= k
                MultiSelToggle -> do
                    engineMode .= duplicateMode gui
                    (liftIO $ Gtk.toggleButtonGetActive (guiMultiSelToggle gui)) >>= k
        susp (SetToggleActive t b k) = do
            case t of
                DrawToggle -> do
                    liftIO $ Gtk.toggleButtonSetActive (guiDrawToggle gui) b
                MultiSelToggle ->
                    liftIO $ Gtk.toggleButtonSetActive (guiMultiSelToggle gui) b
            k

        end a = do
            drawing <- use engineDraw
            engineDraw .= False
            s <- get

            liftIO $ do
                writeIORef (_state i) s
                when drawing (Gtk.widgetQueueDraw $ guiDrawingArea gui)

            return a

    evalStateT (foldFree end susp p) s

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
    base  = fromIntegral $ _viewerBaseWidth v
    width = pageWidth (pages ! pIdx)
    zoom  = zoomValues ! zIdx

--------------------------------------------------------------------------------
_getPage :: EngineState -> Viewer -> PageItem
_getPage s v = pages ! pIdx
  where
    pIdx  = _engineCurPage s
    pages = _viewerPages v

--------------------------------------------------------------------------------
getRects :: EngineState -> [Rect]
getRects s =
    let pId   = s ^. engineCurPage
        rects =
            s ^. engineBoards.boardsMap.at pId.traverse.boardRects.to I.elems in
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
    let gui  = _gui i
        env  = envNew { _enginePageCount = v ^. viewerPageCount
                      , _engineFilename  = takeFileName path
                      }
        name = _engineFilename env
        nb   = v ^. viewerPageCount
        s'   = (stateNew gui) { _engineOverlap = s ^. engineOverlap
                              , _engineBoards  = boardsNew nb
                              }
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

    let v = Viewer{ _viewerDocument  = doc
                  , _viewerPages     = pages
                  , _viewerPageCount = nb
                  , _viewerBaseWidth = 777
                  , _viewerThick     = 1
                  }
    return v

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

--------------------------------------------------------------------------------
trimString :: String -> String
trimString = dropWhileEnd isSpace . dropWhile isSpace
