{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
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
import Dhek.Mode.Selection
import Dhek.Types

--------------------------------------------------------------------------------
data Interpreter
    = Interpreter
      { _internal   :: IORef (Maybe Viewer)
      , _state      :: IORef EngineState
      , _env        :: IORef EngineEnv
      , _gui        :: GUI
      , _modes      :: Modes
      , _curModeMgr :: IORef ModeManager
      }

--------------------------------------------------------------------------------
data Modes
    = Modes
      { modeDraw        :: IO ModeManager
      , modeDuplication :: IO ModeManager
      , modeSelection   :: IO ModeManager
      }

--------------------------------------------------------------------------------
drawInterpret :: (DrawEnv -> M a) -> Interpreter -> Pos -> IO ()
drawInterpret k i (x,y) = do
    s   <- readIORef $ _state i
    opt <- readIORef $ _internal i
    mgr <- readIORef $ _curModeMgr i

    for_ opt $ \v -> do
        let gui   = _gui i
            ratio = _engineRatio s v
            pid   = s ^. engineCurPage
            opts  = DrawEnv
                    { drawOverlap = s ^. engineOverlap
                    , drawPointer = (x/ratio, y/ratio)
                    , drawRects   = getRects s
                    , drawRatio   = ratio
                    }

        s2 <- runMode (mgrMode mgr) s (k opts)
        writeIORef (_state i) s2
        liftIO $ Gtk.widgetQueueDraw $ guiDrawingArea gui

--------------------------------------------------------------------------------
engineRunDraw :: Interpreter -> IO ()
engineRunDraw i = do
    s   <- readIORef $ _state i
    opt <- readIORef $ _internal i
    mgr <- readIORef $ _curModeMgr i
    for_ opt $ \v -> do
        let pages = v ^. viewerPages
            page  = pages ! (s ^. engineCurPage)
            ratio = _engineRatio s v
        s2 <- runMode (mgrMode mgr) s (drawing page ratio)
        writeIORef (_state i) s2

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
engineCurrentPage :: Interpreter -> IO (Maybe PageItem)
engineCurrentPage  i = do
    opt <- readIORef $ _internal i
    s   <- readIORef $ _state i
    return $ fmap (_engineCurrentPage s) opt

--------------------------------------------------------------------------------
_engineCurrentPage :: EngineState -> Viewer -> PageItem
_engineCurrentPage s v =
     let pages = v ^. viewerPages
         pid   = s ^. engineCurPage in
     pages ! pid

--------------------------------------------------------------------------------
engineDrawingArea :: Interpreter -> Gtk.DrawingArea
engineDrawingArea = guiDrawingArea . _gui

--------------------------------------------------------------------------------
-- | Changes engine internal mode
--
--   Internal:
--   --------
--
--   Gets both @EngineState@ and @EngineEnv@ references. Then we call
--   @ModeManager@ cleanup handler of the previous mode. We get a new
--   @EngineState@ out of cleanup handler. That new state is used to store
--   the new @ModeManager@
engineSetMode :: DhekMode -> Interpreter -> IO ()
engineSetMode m i = do
    s       <- readIORef $ _state i
    e       <- readIORef $ _env i
    prevMgr <- readIORef $ _curModeMgr i
    let cleanup = mgrCleanup prevMgr
    s2  <- execStateT cleanup s
    mgr <- selector modes
    writeIORef (_state i) s2
    writeIORef (_curModeMgr i) mgr
    Gtk.widgetQueueDraw area

  where
    modes    = _modes i
    area     = guiDrawingArea $ _gui i
    selector = case m of
        DhekNormal      -> modeDraw
        DhekDuplication -> modeDuplication
        DhekSelection   -> modeSelection

--------------------------------------------------------------------------------
-- | Returns the current page ratio. Returns Nothing if no PDF has been loaded
--   yet.
engineRatio :: Interpreter -> IO (Maybe Double)
engineRatio i = do
    opt <- readIORef $ _internal i
    s   <- readIORef $ _state i
    return $ fmap (_engineRatio s) opt

--------------------------------------------------------------------------------
_engineRatio :: EngineState -> Viewer -> Double
_engineRatio s v =
    let pages = v ^. viewerPages
        zoom  = zoomValues ! (s ^. engineCurZoom)
        pid   = s ^. engineCurPage
        width = pageWidth (pages ! pid)
        base  = fromIntegral (s ^. engineBaseWidth) in
    (base * zoom) / width

--------------------------------------------------------------------------------
makeInterpreter :: GUI -> IO Interpreter
makeInterpreter gui = do
    let env = EngineEnv { _engineFilename = "" }
    eRef <- newIORef env
    sRef <- newIORef stateNew
    vRef <- newIORef Nothing

    -- Instanciates ModeManagers
    let mgrNormal      = normalModeManager gui
        mgrDuplication = duplicateModeManager gui
        mgrSelection   = selectionModeManager (withContext sRef) gui
        modes = Modes
                { modeDraw        = mgrNormal
                , modeDuplication = mgrDuplication
                , modeSelection   = mgrSelection
                }

    curMgr <- mgrNormal
    cRef   <- newIORef curMgr
    return Interpreter{ _internal   = vRef
                      , _state      = sRef
                      , _env        = eRef
                      , _gui        = gui
                      , _modes      = modes
                      , _curModeMgr = cRef
                      }

--------------------------------------------------------------------------------
stateNew :: EngineState
stateNew
    = EngineState
      { _engineCurPage   = 1
      , _engineCurZoom   = 3
      , _engineRectId    = 0
      , _engineOverlap   = False
      , _engineDraw      = False
      , _enginePropLabel = ""
      , _enginePropType  = Nothing
      , _enginePrevPos   = (negate 1, negate 1)
      , _engineDrawState = drawStateNew
      , _engineBoards    = boardsNew 1
      , _engineBaseWidth = 777
      , _engineThick     = 1
      }

--------------------------------------------------------------------------------
runProgram :: Interpreter -> DhekProgram a -> IO (Maybe a)
runProgram i p = do
    s     <- readIORef $ _state i
    opt   <- readIORef $ _internal i
    env   <- readIORef $ _env i

    for opt $ \ v ->
        evalStateT (_evalProgram env (_gui i) (_state i) p v) s

--------------------------------------------------------------------------------
withContext :: IORef EngineState -> (forall m. EngineCtx m => m a) -> IO ()
withContext ref state = do
    s  <- readIORef ref
    s' <- execStateT state s
    writeIORef ref s'

--------------------------------------------------------------------------------
_evalProgram :: EngineEnv
             -> GUI
             -> IORef EngineState
             -> DhekProgram a
             -> Viewer
             -> StateT EngineState IO a
_evalProgram env gui ref prg v= foldFree end susp prg where
  nb = v ^. viewerPageCount

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
  susp (GetRectangles k) = get >>= k . getRects
  susp (IncrPage k) = do
      engineDrawState.drawSelected .= Nothing
      ncur <- engineCurPage <+= 1
      s    <- get

      liftIO $ gtkIncrPage ncur nb (getRects s) gui
      k
  susp (DecrPage k) = do
      engineDrawState.drawSelected .= Nothing
      ncur <- engineCurPage <-= 1
      s    <- get

      liftIO $ gtkDecrPage ncur 1 (getRects s) gui
      k
  susp (IncrZoom k) = do
      ncur <- engineCurZoom <+= 1

      liftIO $ gtkIncrZoom ncur 10 gui
      k
  susp (DecrZoom k) = do
      ncur <- engineCurZoom <-= 1

      liftIO $ gtkDecrZoom ncur 1 gui
      k
  susp (RemoveRect r k) = do
      let id = r ^. rectId

      pid <- use engineCurPage
      engineBoards.boardsMap.at pid.traverse.boardRects.at id .= Nothing

      liftIO $ gtkRemoveRect r gui
      k
  susp (GetEntryText e k) =
      case e of
          PropEntry -> do
              r <- liftIO $ gtkLookupEntryText $ guiNameEntry gui
              k r
          ValueEntry -> do
              r <- liftIO $ gtkLookupEntryText $ guiValueEntry gui
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
  susp (GetFilename k) = k $ _engineFilename env
  susp (ShowError e k) = do
            liftIO $ gtkShowError e gui
            k
  susp (PerformIO action k) = (liftIO action) >>= k
  susp (GetTreeSelection k) = do
      rOpt <- liftIO $ gtkGetTreeSelection gui
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
      r <- liftIO $ gtkSelectJsonFile gui
      k r
  susp (GetAllRects k) = do
      let tup (i, b) = (i, b ^. boardRects.to I.elems)
          list       = fmap tup . I.toList
      (use $ engineBoards.boardsMap.to list) >>= k
  susp (OpenJsonFile k) = do
      r <- liftIO $ gtkOpenJsonFile gui
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

      liftIO $ gtkSetRects (getRects s) gui
      k
  susp (Active o b k) =
      case o of
          Overlap -> do
              engineOverlap .= b
              liftIO $ gtkSetOverlapActive b gui
              k
  susp (IsActive o k) =
      case o of
          Overlap -> (use engineOverlap) >>= k
  susp (SetValuePropVisible b k) = do
      liftIO $ gtkSetValuePropVisible b gui
      k

  end a = do
      drawing <- use engineDraw
      engineDraw .= False
      s <- get

      liftIO $ do
          writeIORef ref s
          when drawing (Gtk.widgetQueueDraw $ guiDrawingArea gui)

      return a

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
    base  = fromIntegral $ (s ^. engineBaseWidth)
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
    opt <- readIORef $ _internal i
    v   <- _loadPdf path
    s   <- readIORef $ _state i
    ev  <- readIORef $ _env i
    case opt of
        Nothing -> do
            let modes = array (1,3)
                        [ (1, normalModeManager gui)
                        , (2, duplicateModeManager gui)
                        , (3, selectionModeManager (withContext $ _state i) gui)
                        ]
                env  = EngineEnv { _engineFilename = takeFileName path }
                name = _engineFilename env
                nb   = v ^. viewerPageCount
                s'   = s & engineBoards .~ boardsNew nb
            writeIORef (_internal i) (Just v)
            writeIORef (_env i) env
            writeIORef (_state i) s'
            ahbox <- Gtk.alignmentNew 0 0 1 1
            Gtk.containerAdd ahbox (guiWindowHBox gui)
            Gtk.boxPackStart (guiWindowVBox gui) ahbox Gtk.PackGrow 0
            Gtk.widgetSetSensitive (guiJsonOpenMenuItem gui) True
            Gtk.widgetSetSensitive (guiJsonSaveMenuItem gui) True
            Gtk.widgetSetSensitive (guiOverlapMenuItem gui) True
            Gtk.widgetSetSensitive (guiPrevButton gui) False
            Gtk.widgetSetSensitive (guiNextButton gui) (nb /= 1)
            Gtk.windowSetTitle (guiWindow gui)
                (name ++ " (page 1 / " ++ show nb ++ ")")
            Gtk.widgetShowAll ahbox
        Just _ -> do
            let env  = ev { _engineFilename  = takeFileName path }
                name = _engineFilename env
                nb   = v ^. viewerPageCount
                ds   = s ^. engineDrawState
                s'   = s { _engineOverlap   = s ^. engineOverlap
                         , _engineBoards    = boardsNew nb
                         , _engineCurPage   = 1
                         , _engineCurZoom   = 3
                         , _engineDrawState = ds { _drawFreshId = 0 }
                         }
            writeIORef (_internal i) (Just v)
            writeIORef (_env i) env
            writeIORef (_state i) s'
            Gtk.listStoreClear $ guiRectStore gui
            Gtk.widgetSetSensitive (guiJsonOpenMenuItem gui) True
            Gtk.widgetSetSensitive (guiJsonSaveMenuItem gui) True
            Gtk.widgetSetSensitive (guiOverlapMenuItem gui) True
            Gtk.widgetSetSensitive (guiPrevButton gui) False
            Gtk.widgetSetSensitive (guiNextButton gui) (nb /= 1)
            Gtk.windowSetTitle (guiWindow gui)
                (name ++ " (page 1 / " ++ show nb ++ ")")
            Gtk.widgetQueueDraw $ guiDrawingArea gui
  where
    gui = _gui i
--------------------------------------------------------------------------------
_loadPdf :: FilePath -> IO Viewer
_loadPdf path = do
    doc   <- fmap fromJust (Poppler.documentNewFromFile path Nothing)
    nb    <- Poppler.documentGetNPages doc
    pages <- _loadPages doc

    let v = Viewer{ _viewerDocument  = doc
                  , _viewerPages     = pages
                  , _viewerPageCount = nb
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
