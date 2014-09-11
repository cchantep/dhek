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
import           Control.Applicative
import           Data.Foldable (find, for_, mapM_)
import           Data.IORef
import qualified Data.IntMap as I
import           Data.Traversable

--------------------------------------------------------------------------------
import           Control.Lens      hiding (Action, act)
import           Control.Monad.RWS hiding (mapM_)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk          as Gtk

--------------------------------------------------------------------------------
import Dhek.Cartesian
import Dhek.Engine.Instr
import Dhek.Engine.Type
import Dhek.Geometry
import Dhek.GUI
import Dhek.GUI.Action
import Dhek.I18N
import Dhek.Mode.Common.Draw
import Dhek.Mode.DuplicateKey
import Dhek.Mode.Effect.Collision
import Dhek.Mode.Effect.Magnetic
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
data Effect
    = Col Collide
    | Att Attract
    | None

--------------------------------------------------------------------------------
data Transform
    = Moving Rect Effect Vector2D
    | Resizing Rect Area Vector2D

--------------------------------------------------------------------------------
data Action
    = Drawing Vector2D
    | Transform Transform
    | MoveGuide Guide

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
magneticForce :: Double -> Double
magneticForce ratio = 25 / ratio

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
    = do magnet <- use engineMagnetic
         if magnet
             then magneticAction env act
             else normalActionDispatch env act

--------------------------------------------------------------------------------
normalActionDispatch :: DrawEnv -> Action -> NormalMode Action
normalActionDispatch env act
    = case act of
          Drawing v   -> normalDrawSelection env v
          MoveGuide g -> normalMoveGuide g
          Transform t -> normalTransform env t

--------------------------------------------------------------------------------
magneticAttract :: DrawEnv -> Action -> NormalMode (Maybe Action)
magneticAttract env act
    = do gs <- engineStateGetGuides
         let ls = fmap toLine gs
         case act of
             Drawing v
                 -> return $ attractDraw env v ls
             MoveGuide _
                 -> return Nothing
             Transform t
                 -> case t of
                        Moving r _ v
                            -> return $ attractMove env r v ls
                        Resizing r a v
                            -> return $ attractResize env r a v ls
  where
    toLine g
        = case g ^. guideType of
              GuideVertical   -> verticalLine (g ^. guideValue)
              GuideHorizontal -> horizontalLine (g ^. guideValue)

--------------------------------------------------------------------------------
attractMove :: DrawEnv -> Rect -> Vector2D -> [Line] -> Maybe Action
attractMove env r v ls
    = fmap go mAtt
  where
    go att  = Transform $ Moving r (Att att) newVect
    mAtt    = magneticAttraction (magneticMove r newVect) force ls
    pt      = drawPointer env
    ratio   = drawRatio env
    newVect = v & vectorTo .~ pt
    force   = magneticForce ratio

--------------------------------------------------------------------------------
attractDraw :: DrawEnv -> Vector2D -> [Line] -> Maybe Action
attractDraw _ _ _
    = Nothing
  -- where
  --   mAtt = magneticAttraction (magneticDraw v) ls

--------------------------------------------------------------------------------
attractResize :: DrawEnv -> Rect -> Area -> Vector2D -> [Line] -> Maybe Action
attractResize _ _ _ _ _
    = Nothing
  -- where
  --   mAtt = magneticAttraction (magneticResize r a v) ls

--------------------------------------------------------------------------------
magneticAction :: DrawEnv -> Action -> NormalMode Action
magneticAction env act
    = do mAtt <- magneticAttract env act
         case mAtt of
             Nothing     -> normalActionDispatch env act
             Just newAct -> return newAct

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
          Transform t ->
              case t of
                  Moving _ _ _   -> Just $ GTKCursor Gtk.Hand1
                  Resizing _ a _ -> Just $ GTKCursor $ areaCursor a
          MoveGuide _ -> Just $ GTKCursor Gtk.Hand1
          _           -> Nothing

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
normalTransform :: DrawEnv -> Transform -> NormalMode Action
normalTransform env t
    = do overlap <- use engineOverlap
         if overlap
             then
             case t of
                 Moving r _ v   -> normalOverlapMove env r v
                 Resizing r a v -> normalOverlapResize env r a v
             else
             case t of
                 Moving r _ v   -> normalCollisionMove env r v
                 Resizing r a v -> normalOverlapResize env r a v

--------------------------------------------------------------------------------
normalOverlapMove :: DrawEnv -> Rect -> Vector2D -> NormalMode Action
normalOverlapMove env r v = return $ Transform $ Moving r None newVect
  where
    pos     = drawPointer env
    newVect = v & vectorTo .~ pos

--------------------------------------------------------------------------------
normalOverlapResize :: DrawEnv -> Rect -> Area -> Vector2D -> NormalMode Action
normalOverlapResize env r a v = return $ Transform $ Resizing r a newVect
  where
    pos     = drawPointer env
    newVect = v & vectorTo .~ pos

--------------------------------------------------------------------------------
normalCollisionMove :: DrawEnv -> Rect -> Vector2D -> NormalMode Action
normalCollisionMove env r v
    = case inter of
        Nothing
            -> normalOverlapMove env r v
        Just c
            -> return $ Transform $ Moving r (Col c) newVect
  where
    pos     = drawPointer env
    newVect = v & vectorTo .~ pos
    rs      = drawRects env
    inter   = collisionCollide (collisionMove r newVect) rs

--------------------------------------------------------------------------------
normalDrawSelection :: DrawEnv -> Vector2D -> NormalMode Action
normalDrawSelection env v
    = return $ Drawing newVect
  where
    pos     = drawPointer env
    newVect = v & vectorTo .~ pos

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
    = maybe moving resizing aOpt
  where
    pos        = drawPointer env
    aOpt       = getOverArea env
    newVect    = vector2D pos pos
    moving     = Transform $ Moving r None newVect
    resizing a = Transform $ Resizing r a newVect

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
          Nothing -> (Drawing vect, guides)
  where
    (gOpt, newGuides)  = findOveredGuide guides
    findOveredGuide gs = findDelete (isOverGuide gRange env) gs
    gRange             = guideRange env
    pt                 = drawPointer env
    vect               = vector2D pt pt

--------------------------------------------------------------------------------
normalPressNoRect :: DrawEnv -> NormalMode ()
normalPressNoRect env
    = do gs     <- engineStateGetGuides
         actRef <- asks inputAction
         let (act, gs') = normalNoRectDetectAction env gs

         liftIO $ writeIORef actRef $ Just act
         engineStateSetGuides gs'

--------------------------------------------------------------------------------
normalTransformRelease :: Transform -> NormalMode ()
normalTransformRelease t
    = do pid <- use engineCurPage

         let r = case t of
                 Moving r' mC v
                     -> case mC of
                             None
                                 -> moveRect v r'
                             Col c
                                 -> projectCollision (collisionMove r' v ) c
                             Att a
                                 -> projectMagnetic (magneticMove r' v) a
                 Resizing r' a v
                     -> normalize $ resizeRect a v r'
             rid = r ^. rectId

         engineBoards.boardsMap.at pid.traverse.boardRects.at rid ?= r
         engineEventStack %= (UpdateRectPos:)

         normalSelectRectangle r

--------------------------------------------------------------------------------
normalDrawingRelease :: Vector2D -> NormalMode ()
normalDrawingRelease v
    = when (w*h >= 30) $
          do rid <- engineDrawState.drawFreshId <+= 1
             let r1 = r & rectId   .~ rid
                        & rectName %~ (++ show rid)

             -- New rectangle
             gui <- asks inputGUI
             pid <- use engineCurPage

             engineBoards.boardsMap.at pid.traverse.boardRects.at rid ?= r1
             engineEventStack %= (CreateRect:)

             liftIO $ gtkAddRect r1 gui
             normalSelectRectangle r1
  where
    r = makeDrawSelectionRect v
    w = r ^. rectWidth
    h = r ^. rectHeight

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
                 Transform t -> normalTransformRelease t --  r
                 Drawing v   -> normalDrawingRelease v
                 MoveGuide g -> normalGuideRelease g

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
                     { inputGUI         = gui
                     , inputCurModeMgr  = mgrRef
                     , inputAction      = actRef
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
transGetRect (Transform t)
    = case t of
          Moving r mC v
              -> case mC of
                      None  -> Just $ moveRect v r
                      Col c -> Just $ projectCollision (collisionMove r v) c
                      Att a -> Just $ projectMagnetic (magneticMove r v) a
          Resizing r a v -> Just $ resizeRect a v r
transGetRect _ = Nothing

--------------------------------------------------------------------------------
drawingGetRect :: Action -> Maybe Rect
drawingGetRect (Drawing v) = Just $ makeDrawSelectionRect v
drawingGetRect _           = Nothing

--------------------------------------------------------------------------------
actionGetGuide :: Action -> Maybe Guide
actionGetGuide (MoveGuide g) = Just g
actionGetGuide _             = Nothing
