{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Mode.Normal
--
--------------------------------------------------------------------------------
module Dhek.Mode.Normal (normalModeManager) where

--------------------------------------------------------------------------------
import Prelude hiding (mapM_)

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Foldable (find, for_, mapM_)
import Data.IORef
import Data.Traversable

--------------------------------------------------------------------------------
import           Control.Lens hiding (Action, act)
import           Control.Monad.RWS hiding (mapM_)
import qualified Data.IntMap                  as I
import qualified Graphics.Rendering.Cairo     as Cairo
import qualified Graphics.UI.Gtk              as Gtk

--------------------------------------------------------------------------------
import Dhek.Engine.Instr
import Dhek.Engine.Type
import Dhek.Geometry
import Dhek.GUI
import Dhek.GUI.Action
import Dhek.I18N
import Dhek.Mode.Common.Draw
import Dhek.Mode.DuplicateKey
import Dhek.Types
import Dhek.Utils (findDelete)

--------------------------------------------------------------------------------
data Input
    = Input
      { inputGUI        :: GUI
      , inputCurModeMgr :: IORef (Maybe ModeManager)
      , inputAction     :: IORef (Maybe Action)
      }

--------------------------------------------------------------------------------
data Action
    = Drawing Rect
    | Moving Pos (Maybe Collision) Rect
    | Resizing Pos Area Rect
    | MoveGuide Guide

--------------------------------------------------------------------------------
data Collision
    = Collision
      { colDelta     :: !Double
      , colRange     :: !(Double, Double)
      , colPrevPos   :: !(Double, Double)
      , colDirection :: !Direction
      }

--------------------------------------------------------------------------------
newtype NormalMode a
    = NormalMode (RWST Input () EngineState IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Input
             , MonadState EngineState
             , MonadIO
             )

--------------------------------------------------------------------------------
instance ModeMonad NormalMode where
    mMove opts
        = runOrMode (normalMove opts) (move opts)

    mPress opts
        = runOrMode (normalPress opts) (press opts)

    mRelease opts
        = runOrMode (normalRelease opts) (release opts)

    mDrawing page ratio
        = runOrMode (normalDrawing page ratio) (drawing page ratio)

    mKeyPress kb
        = do input <- ask
             let g   = inputGUI input
                 ref = inputCurModeMgr input
             opt <- liftIO $ readIORef ref
             case opt of
                 Just _ -> return ()
                 -- Duplication mode use ALT modifier
                 _ -> when (statusNamePressed $ kbKeyName kb) $ liftIO $
                          do mgr <- duplicateKeyModeManager g
                             writeIORef ref $ Just mgr
                             isOverDrawingArea <- overDrawingArea g
                             when isOverDrawingArea $
                                 do _ <- Gtk.statusbarPush (guiStatusBar g)
                                         (guiContextId g)
                                         (guiTranslate g MsgDupHelp)
                                    Gtk.widgetShowAll $ guiDrawPopup g
                                    updatePopupPos g
                                    gtkSetDhekCursor g (Just $ DhekCursor CursorDup)

    mKeyRelease kb
        = do input <- ask
             let g   = inputGUI input
                 ref = inputCurModeMgr input
             opt <- liftIO $ readIORef ref
             case opt of
                 Nothing -> return ()
                 -- In Duplication mode
                 Just mgr
                     -> when (statusNamePressed $ kbKeyName kb) $
                            do mgrCleanup mgr
                               liftIO $
                                   do Gtk.statusbarPop (guiStatusBar g)
                                          (guiContextId g)
                                      Gtk.widgetHide $ guiDrawPopup g
                                      writeIORef ref Nothing
                                      gtkSetDhekCursor g Nothing

    mEnter
        = do input <- ask
             let g   = inputGUI input
                 ref = inputCurModeMgr input
             opt <- liftIO $ readIORef ref
             case opt of
                 Nothing -> return ()
                 -- In duplication mode
                 Just _
                     -> liftIO $
                            do Gtk.widgetShowAll $ guiDrawPopup g
                               updatePopupPos g

    mLeave
        = do input <- ask
             let g   = inputGUI input
                 ref = inputCurModeMgr input
             opt <- liftIO $ readIORef ref
             case opt of
                 Nothing -> return ()
                 -- In duplication mode
                 Just _
                     -> liftIO $ Gtk.widgetHide $ guiDrawPopup g

--------------------------------------------------------------------------------
runOrMode :: NormalMode () -> M () -> NormalMode ()
runOrMode nm m
    = do ref <- asks inputCurModeMgr
         opt <- liftIO $ readIORef ref
         case opt of
             Nothing
                 -> nm
             Just mgr
                 -> do s <- get
                       let (Mode k) = mgrMode mgr
                       s' <- liftIO $ k m s
                       put s'

--------------------------------------------------------------------------------
normalMove :: DrawEnv -> NormalMode ()
normalMove env
    = do actRef  <- asks inputAction
         mAct    <- liftIO $ readIORef actRef
         mNewAct <- traverse (normalOnAction env) mAct

         liftIO $ writeIORef actRef mNewAct
         maybe (normalNoAction env) normalSetActionCursor mNewAct

--------------------------------------------------------------------------------
normalOnAction :: DrawEnv -> Action -> NormalMode Action
normalOnAction env act
    = case act of
          Drawing r   -> normalDrawSelection env r
          MoveGuide g -> normalMoveGuide g
          _           -> normalMoveOrResize env act

--------------------------------------------------------------------------------
normalSetActionCursor :: Action -> NormalMode ()
normalSetActionCursor act
    = do gui <- asks inputGUI
         liftIO $ gtkSetDhekCursor gui $ normalActionCursor act

--------------------------------------------------------------------------------
normalNoAction :: DrawEnv -> NormalMode ()
normalNoAction env
    = do gui <- asks inputGUI
         gs  <- engineStateGetGuides

         let ogOpt = find (isOverGuide gRange env) gs
         engineDrawState.drawOverRect  .= oOpt
         engineDrawState.drawOverGuide .= ogOpt
         liftIO $ gtkSetDhekCursor gui $ cursor ogOpt
  where
    oOpt         = getOverRect env
    aOpt         = getOverArea env
    gRange       = guideRange env
    cursor ogOpt = fmap (GTKCursor . areaCursor) aOpt <|>
                   (GTKCursor Gtk.Hand1) <$ oOpt      <|>
                   (GTKCursor Gtk.Hand1) <$ ogOpt

--------------------------------------------------------------------------------
normalActionCursor :: Action -> Maybe DhekCursor
normalActionCursor act
    = case act of
          Moving _ _ _   -> Just $ GTKCursor Gtk.Hand1
          Resizing _ a _ -> Just $ GTKCursor $ areaCursor a
          MoveGuide _    -> Just $ GTKCursor Gtk.Hand1
          _              -> Nothing

--------------------------------------------------------------------------------
normalMoveGuide :: Guide -> NormalMode Action
normalMoveGuide g
    = do gui <- asks inputGUI
         x   <- liftIO $ Gtk.get (guiHRuler gui) Gtk.rulerPosition
         y   <- liftIO $ Gtk.get (guiVRuler gui) Gtk.rulerPosition

         engineDrawState.drawOverGuide .= Nothing

         let newValue =
                 case g ^. guideType of
                     GuideVertical   -> x
                     GuideHorizontal -> y
         return $ MoveGuide (g & guideValue .~ newValue)

--------------------------------------------------------------------------------
normalMoveOrResize :: DrawEnv -> Action -> NormalMode Action
normalMoveOrResize env act
    = do overlap <- use engineOverlap
         if overlap
             then
             case act of
                 Moving ppos _ r   -> normalOverlapMove ppos pos r
                 Resizing ppos a r -> normalOverlapResize ppos pos a r
                 _                 -> error "impossible normalMoveOrResize"
             else
             case act of
                 Moving ppos c r   -> normalCollisionMove env ppos c r
                 Resizing ppos a r -> normalOverlapResize ppos pos a r
                 _                 -> error "impossible normalMoveOrResize"
  where
    pos = drawPointer env

--------------------------------------------------------------------------------
normalOverlapMove :: Pos -> Pos -> Rect -> NormalMode Action
normalOverlapMove ppos pos r
    = return $ Moving pos Nothing $ moveRect ppos pos r

--------------------------------------------------------------------------------
normalOverlapResize :: Pos -> Pos -> Area -> Rect -> NormalMode Action
normalOverlapResize (x0,y0) pos@(x,y) a r
    = let dx = x-x0
          dy = y-y0 in
      return $ Resizing pos a (resizeRect dx dy a r)

--------------------------------------------------------------------------------
normalCollisionMove :: DrawEnv
                    -> Pos
                    -> Maybe Collision
                    -> Rect
                    -> NormalMode Action
normalCollisionMove env ppos mC r
    = maybe noPreviousCollision withPreviousCollision mC
  where
    pos@(x,y) = drawPointer env

    noPreviousCollision
        = case intersection (drawRects env) r of
              Nothing -> normalOverlapMove ppos pos r
              Just (t,d)
                  -> let delta  = collisionDelta d r t
                         newPos = adaptPos d delta x y
                         r1     = adaptRect d delta r
                         c      = Collision
                                  { colDelta     = delta
                                  , colRange     = rectRange d t
                                  , colPrevPos   = ppos
                                  , colDirection = d
                                  } in
                     return $ Moving newPos (Just c) r1

    withPreviousCollision c
        = let (rmin,rmax) = colRange c
              prevColPos  = colPrevPos c
              d           = colDirection c
              delta       = colDelta c
              collides    = rangeCollides d rmin rmax r
              diff        = diffPos d delta pos prevColPos
              adaptedPos  = adaptPosDefault d delta pos ppos
              r1          = oppositeTranslate d pos ppos r
              newPos      = movePos d pos prevColPos
              catchUp     = diff <= 0 in
          if not catchUp && collides
          then return $ Moving newPos (Just c) r1
          else let (r2, correctedPos)
                       = correctRect d adaptedPos r in
               return $ Moving correctedPos Nothing r2

--------------------------------------------------------------------------------
normalDrawSelection :: DrawEnv -> Rect -> NormalMode Action
normalDrawSelection env s
    = return $ Drawing $ updateDrawSelection pos s
  where
    pos = drawPointer env

--------------------------------------------------------------------------------
-- | if user:
--      - click on nothing: we're in drawing mode
--      - click on a guide: guide is selected
--      - click on a rectangle: we enter in event mode (Resize or Hold)
normalPress :: DrawEnv -> NormalMode ()
normalPress env
    = maybe (normalPressNoRect env) (normalPressRect env) oOpt
  where
    oOpt = getOverRect env

--------------------------------------------------------------------------------
normalDetectAction :: DrawEnv -> Rect -> Action
normalDetectAction env r
    = maybe (Moving pos Nothing r) (\a -> Resizing pos a r) aOpt
  where
    pos  = drawPointer env
    aOpt = getOverArea env

--------------------------------------------------------------------------------
normalPressRect :: DrawEnv -> Rect -> NormalMode ()
normalPressRect env r
    = do pid    <- use engineCurPage
         actRef <- asks inputAction

         engineBoards.boardsMap.at pid.traverse.boardRects.at rid .= Nothing

         liftIO $ writeIORef actRef $ Just act
  where
    act = normalDetectAction env r
    rid = r ^. rectId

--------------------------------------------------------------------------------
normalNoRectDetectAction :: DrawEnv -> [Guide] -> (Action, [Guide])
normalNoRectDetectAction env guides
    = case gOpt of
          Just g  -> (MoveGuide g, newGuides)
          Nothing -> (Drawing newSelection, guides)
  where
    (gOpt, newGuides)  = findOveredGuide guides
    findOveredGuide gs = findDelete (isOverGuide gRange env) gs
    gRange             = guideRange env
    (x,y)              = drawPointer env
    newSelection       = rectNew x y 0 0

--------------------------------------------------------------------------------
normalPressNoRect :: DrawEnv -> NormalMode ()
normalPressNoRect env
    = do gs     <- engineStateGetGuides
         actRef <- asks inputAction
         let (act, gs') = normalNoRectDetectAction env gs

         liftIO $ writeIORef actRef $ Just act
         engineStateSetGuides gs'


--------------------------------------------------------------------------------
normalMoveOrResizeRelease :: Rect -> NormalMode ()
normalMoveOrResizeRelease r
    = do pid <- use engineCurPage

         engineBoards.boardsMap.at pid.traverse.boardRects.at rid ?= r
         engineEventStack %= (UpdateRectPos:)

         normalSelectRectangle r
  where
    rid = r ^. rectId

--------------------------------------------------------------------------------
normalDrawingRelease :: Rect -> NormalMode ()
normalDrawingRelease r
    = when (w*h >= 30) $
          do rid <- engineDrawState.drawFreshId <+= 1
             let r2 = r1 & rectId   .~ rid
                         & rectName %~ (++ show rid)

             -- New rectangle
             gui <- asks inputGUI
             pid <- use engineCurPage

             engineBoards.boardsMap.at pid.traverse.boardRects.at rid ?= r2
             engineEventStack %= (CreateRect:)

             liftIO $ gtkAddRect r2 gui
             normalSelectRectangle r2
  where
    r1 = normalize r
    w  = r1 ^. rectWidth
    h  = r1 ^. rectHeight

--------------------------------------------------------------------------------
normalGuideRelease :: Guide -> NormalMode ()
normalGuideRelease g
    = do gs <- engineStateGetGuides
         engineStateSetGuides (g:gs)

--------------------------------------------------------------------------------
normalRelease :: DrawEnv -> NormalMode ()
normalRelease _
    = do actRef <- asks inputAction
         mAct   <- liftIO $ readIORef actRef

         for_ mAct $ \act ->
             case act of
                 Moving _ _ r   -> normalMoveOrResizeRelease r
                 Resizing _ _ r -> normalMoveOrResizeRelease r
                 Drawing r      -> normalDrawingRelease r
                 MoveGuide g    -> normalGuideRelease g

         liftIO $ writeIORef actRef Nothing

--------------------------------------------------------------------------------
normalDrawing :: PageItem -> Ratio -> NormalMode ()
normalDrawing page ratio
    = do gui    <- asks inputGUI
         actRef <- asks inputAction
         mAct   <- liftIO $ readIORef actRef
         ds     <- use $ engineDrawState
         pid    <- use $ engineCurPage
         bd     <- use $ engineBoards.boardsMap.at pid.traverse
         let --guides    = bd ^. boardGuides
             curGuide  = mAct >>= actionGetGuide
             rects     = bd ^. boardRects.to I.elems
             overGuide = ds ^. drawOverGuide

         liftIO $ do
             frame     <- Gtk.widgetGetDrawWindow $ guiDrawingArea gui
             (fw',fh') <- Gtk.drawableGetSize frame

             let width  = ratio * (pageWidth page)
                 height = ratio * (pageHeight page)
                 fw     = fromIntegral fw'
                 fh     = fromIntegral fh'
                 eventR = mAct >>= transGetRect
                 area   = guiDrawingArea gui

             Gtk.widgetSetSizeRequest area (truncate width) (truncate height)
             Gtk.renderWithDrawable frame $ do
                 suf <- guiPdfSurface page ratio gui
                 Cairo.setSourceSurface suf 0 0
                 Cairo.paint

                 Cairo.scale ratio ratio
                 mapM_ (drawGuide fw fh rgbGreen) overGuide
                 Cairo.closePath
                 Cairo.stroke
                 mapM_ (drawGuide fw fh selectedColor) curGuide
                 Cairo.closePath
                 Cairo.stroke

                 -- Draw the entire board
                 for_ rects $ \r ->
                     case () of
                         _ | Just r == ds ^. drawSelected -> do
                             drawRect selectedColor Line r

                           | Just r == ds ^. drawOverRect ->
                             drawRect overedColor Line r

                           | otherwise ->
                               drawRect regularColor Line r

                 -- Draw drawing selection rectangle
                 for_ (mAct >>= drawingGetRect) $ \r -> do
                     drawRect selectionColor Line r
                     drawRectGuides fw fh rectGuideColor r

                 -- Draw event rectangle
                 for_ eventR $ \r -> do
                     drawRect selectedColor Line r
                     drawRectGuides fw fh rectGuideColor r
  where
    overedColor    = RGB 0.16 0.72 0.92
    regularColor   = rgbBlue
    selectedColor  = rgbRed
    selectionColor = rgbGreen
    rectGuideColor = RGB 0.16 0.72 0.92
    --guideColor     = RGB 0.16 0.26 0.87

--------------------------------------------------------------------------------
runNormal :: Input -> NormalMode a -> EngineState -> IO EngineState
runNormal input (NormalMode m) s
    = do (s', _) <- execRWST m input s
         return s'

--------------------------------------------------------------------------------
normalMode :: Input -> Mode
normalMode gui = Mode (runNormal gui . runM)

--------------------------------------------------------------------------------
normalModeManager :: GUI -> IO ModeManager
normalModeManager gui
    = do mgrRef <- newIORef Nothing
         actRef <- newIORef Nothing

         -- Display normal Help message
         Gtk.statusbarPop (guiStatusBar gui) (guiContextId gui)
         _ <- Gtk.statusbarPush (guiStatusBar gui) (guiContextId gui)
              (guiTranslate gui MsgDrawHelp)

         let input = Input
                     { inputGUI        = gui
                     , inputCurModeMgr = mgrRef
                     , inputAction     = actRef
                     }
         return $ ModeManager (normalMode input) (return ())

--------------------------------------------------------------------------------
normalSelectRectangle :: Rect -> NormalMode ()
normalSelectRectangle r = do
    engineDrawState.drawSelected ?= r
    gui <- asks inputGUI
    liftIO $ gtkSelectRect r gui

--------------------------------------------------------------------------------
-- | Guide range detection in pixels
guideRange :: DrawEnv -> Double
guideRange opts
    = 10 / ratio where ratio = drawRatio opts

--------------------------------------------------------------------------------
statusNamePressed :: String -> Bool
statusNamePressed n
    | "Alt_L" <- n = True
    | "Alt_R" <- n = True
    | otherwise    = False

--------------------------------------------------------------------------------
isOverGtkRect :: (Int, Int) -> Gtk.Rectangle -> Bool
isOverGtkRect (x,y) (Gtk.Rectangle rx ry rw rh)
    = rx <= x && x <= rx + rw &&
      ry <= y && y <= ry + rh

--------------------------------------------------------------------------------
overDrawingArea :: GUI -> IO Bool
overDrawingArea g
    = do pos    <- Gtk.widgetGetPointer $ guiWindow g
         size   <- Gtk.widgetGetSize $ guiDrawingAreaViewport g
         mcoord <- Gtk.widgetTranslateCoordinates
                   (guiDrawingArea g) (guiWindow g) 0 0

         return $ maybe False (calculate pos size) mcoord
  where
    calculate pos (rw, rh) (x,y)
        = isOverGtkRect pos (Gtk.Rectangle x y rw rh)

--------------------------------------------------------------------------------
transGetRect :: Action -> Maybe Rect
transGetRect (Moving _ _ r)   = Just r
transGetRect (Resizing _ _ r) = Just r
transGetRect _                = Nothing

--------------------------------------------------------------------------------
drawingGetRect :: Action -> Maybe Rect
drawingGetRect (Drawing r) = Just r
drawingGetRect _           = Nothing

--------------------------------------------------------------------------------
actionGetGuide :: Action -> Maybe Guide
actionGetGuide (MoveGuide g) = Just g
actionGetGuide _             = Nothing
