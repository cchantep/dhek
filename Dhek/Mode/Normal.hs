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
import Data.Maybe (isJust, isNothing)
import Data.Traversable

--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.RWS hiding (mapM_)
import           Control.Monad.Trans
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
                             overDrawingArea <- overDrawingArea g
                             when overDrawingArea $
                                 do Gtk.statusbarPush (guiStatusBar g)
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
normalMove opts
    = do gs <- engineStateGetGuides

         let oOpt   = getOverRect opts
             aOpt   = getOverArea opts
             gRange = guideRange opts
             ogOpt  = find (isOverGuide gRange opts) gs

         eOpt <- use $ engineDrawState.drawEvent
         sOpt <- use $ engineDrawState.drawSelection
         gOpt <- use $ engineDrawState.drawCurGuide

         engineDrawState.drawOverRect  .= oOpt
         engineDrawState.drawOverGuide .= ogOpt

         gui <- asks inputGUI
         for_ gOpt $ \g ->
             do x <- liftIO $ Gtk.get (guiHRuler gui) Gtk.rulerPosition
                y <- liftIO $ Gtk.get (guiVRuler gui) Gtk.rulerPosition
                let v = case g ^. guideType of
                        GuideVertical   -> x
                        GuideHorizontal -> y

                    g' = g & guideValue .~ v

                engineDrawState.drawCurGuide ?= g'

         -- When user draws a rectangle
         for_ sOpt $ \s -> do
             let pos = drawPointer opts
             engineDrawState.drawSelection ?= updateDrawSelection pos s

         -- When user resizes or moves a rectangle
         for_ eOpt $ \e ->
             do if drawOverlap opts
                    then overlapMode e
                    else collisionMode e

         let cursorOpt
                 = fmap (GTKCursor . eventCursor) eOpt <|>
                   fmap (GTKCursor . areaCursor) aOpt  <|>
                   (GTKCursor Gtk.Hand1) <$ oOpt       <|>
                   (GTKCursor Gtk.Hand1) <$ (gOpt <|> ogOpt)

         -- Update Gtk cursor
         liftIO $ gtkSetDhekCursor gui cursorOpt

  where
    overlapMode e = do
        let pos@(x,y) = drawPointer opts
        case e of
            Hold r ppos
                -> engineDrawState.drawEvent ?=
                       Hold (moveRect ppos pos r) (x,y)
            Resize r (x0,y0) a
                -> let dx = x-x0
                       dy = y-y0 in
                   engineDrawState.drawEvent ?=
                       Resize (resizeRect dx dy a r) (x,y) a

    collisionMode e = do
        let (x,y) = drawPointer opts
        case e of
            Hold r (x0,y0) -> do
                cOpt <- use $ engineDrawState.drawCollision
                case cOpt of
                    Nothing -> -- No previous collision
                        case intersection (drawRects opts) r of
                            Nothing    -> overlapMode e
                            Just (t,d) -> do
                                let delta = collisionDelta d r t
                                    r1    = adaptRect d delta r
                                    c     = Collision
                                            { colDelta     = delta
                                            , colRange     = rectRange d t
                                            , colPrevPos   = (x0,y0)
                                            , colDirection = d
                                            }

                                engineDrawState.drawEvent ?=
                                    Hold r1 (adaptPos d delta x y)
                                engineDrawState.drawCollision ?= c

                    Just c -> do -- with previous collision
                        let (rmin,rmax) = colRange c
                            (px,py)     = colPrevPos c
                            d           = colDirection c
                            delta       = colDelta c
                            collides    = rangeCollides d rmin rmax r
                            diff        = diffPos d delta (x,y) (px,py)
                            (x0',y0')   = adaptPosDefault d delta (x,y) (x0,y0)
                            r1          = oppositeTranslate d (x,y) (x0,y0) r
                            (px',py')   = movePos d (x,y) (px,py)
                            catchUp     = diff <= 0

                        if not catchUp && collides
                            then engineDrawState.drawEvent ?= Hold r1 (px',py')
                            else do
                                engineDrawState.drawCollision .= Nothing
                                let (r2, newPos) = correctRect d (x0',y0') r
                                engineDrawState.drawEvent ?= Hold r2 newPos

            _ -> overlapMode e -- Resize collision detection is
                               -- not supported yet.

--------------------------------------------------------------------------------
normalPress :: DrawEnv -> NormalMode ()
normalPress opts
    = do let (x,y)        = drawPointer opts
             oOpt         = getOverRect opts
             aOpt         = getOverArea opts
             newSelection = rectNew x y 0 0
             gRange       = guideRange opts

             onEvent r = do
                 let rid = r ^. rectId
                     evt = maybe (Hold r (x,y)) (Resize r (x,y)) aOpt
                 engineDrawState.drawEvent ?= evt

                 pid <- use engineCurPage
                 engineBoards.boardsMap.at pid.traverse.boardRects.at rid .=
                     Nothing

             noEvent
                 = do gs <- engineStateGetGuides
                      let (gOpt, gs') = findDelete (isOverGuide gRange opts) gs
                      when (isJust gOpt) $
                          do engineDrawState.drawCurGuide .= gOpt
                             engineStateSetGuides gs'
                      when (isNothing gOpt) $
                          engineDrawState.drawSelection ?= newSelection

         -- if user:
         --      - click on nothing: we're in drawing mode
         --      - click on a guide: guide is selected
         --      - click on a rectangle: we enter in event mode (Resize or Hold)
         maybe noEvent onEvent oOpt

--------------------------------------------------------------------------------
normalRelease :: DrawEnv -> NormalMode ()
normalRelease _
    = do sOpt <- use $ engineDrawState.drawSelection
         eOpt <- use $ engineDrawState.drawEvent
         gOpt <- use $ engineDrawState.drawCurGuide

         -- on event mode, we re-insert targeted rectangle rectangle list
         for_ eOpt $ \e -> do
             let rid = r ^. rectId
                 r   = normalize $ case e of
                     Hold x _     -> x
                     Resize x _ _ -> x

             -- Attach rectangle
             pid <- use engineCurPage
             engineBoards.boardsMap.at pid.traverse.boardRects.at rid ?= r

             normalSelectRectangle r

             engineDrawState.drawEvent     .= Nothing
             engineDrawState.drawCollision .= Nothing
             engineEventStack %= (UpdateRectPos:)

         -- on drawing mode, we add the new rectangle to rectangle list
         for_ sOpt $ \r -> do
             let r1 = normalize r
                 w  = r1 ^. rectWidth
                 h  = r1 ^. rectHeight

             when (w*h >= 30) $ do
                 rid <- engineDrawState.drawFreshId <+= 1
                 let r2 = r1 & rectId   .~ rid
                             & rectName %~ (++ show rid)

                 -- New rectangle
                 gui <- asks inputGUI
                 pid <- use engineCurPage
                 engineBoards.boardsMap.at pid.traverse.boardRects.at rid ?= r2
                 liftIO $ gtkAddRect r2 gui

                 normalSelectRectangle r2

             engineDrawState.drawSelection .= Nothing
             engineEventStack %= (CreateRect:)

         -- on guide update, we insert back the updated guide to its board
         for_ gOpt $ \g ->
             do gs <- engineStateGetGuides
                engineStateSetGuides (g:gs)
                engineDrawState.drawCurGuide .= Nothing

--------------------------------------------------------------------------------
normalDrawing :: PageItem -> Ratio -> NormalMode ()
normalDrawing page ratio
    = do gui <- asks inputGUI
         ds  <- use $ engineDrawState
         pid <- use $ engineCurPage
         bd  <- use $ engineBoards.boardsMap.at pid.traverse
         let guides    = bd ^. boardGuides
             curGuide  = ds ^. drawCurGuide
             rects     = bd ^. boardRects.to I.elems
             overGuide = ds ^. drawOverGuide

         liftIO $ do
             frame     <- Gtk.widgetGetDrawWindow $ guiDrawingArea gui
             (fw',fh') <- Gtk.drawableGetSize frame

             let width  = ratio * (pageWidth page)
                 height = ratio * (pageHeight page)
                 fw     = fromIntegral fw'
                 fh     = fromIntegral fh'
                 eventR = (ds ^. drawEvent) >>= eventGetRect
                 area   = guiDrawingArea gui

             Gtk.widgetSetSizeRequest area (truncate width) (truncate height)
             Gtk.renderWithDrawable frame $ do
                 suf <- guiPdfSurface page ratio gui
                 Cairo.setSourceSurface suf 0 0
                 Cairo.paint

                 Cairo.scale ratio ratio
                 mapM_ (drawGuide fw fh guideColor) guides
                 Cairo.closePath
                 Cairo.stroke
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
                             drawRect fw fh selectedColor Line r

                           | Just r == ds ^. drawOverRect ->
                             drawRect fw fh overedColor Line r

                           | otherwise ->
                               drawRect fw fh regularColor Line r

                 -- Draw drawing selection rectangle
                 for_ (ds ^. drawSelection) $ \r -> do
                     drawRect fw fh selectionColor Line r
                     drawRectGuides fw fh rectGuideColor r

                 -- Draw event rectangle
                 for_ eventR $ \r -> do
                     drawRect fw fh selectedColor Line r
                     drawRectGuides fw fh rectGuideColor r
  where
    overedColor    = RGB 0.16 0.72 0.92
    regularColor   = rgbBlue
    selectedColor  = rgbRed
    selectionColor = rgbGreen
    rectGuideColor = RGB 0.16 0.72 0.92
    guideColor     = RGB 0.16 0.26 0.87

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
    = do ref <- newIORef Nothing

         -- Display normal Help message
         Gtk.statusbarPop (guiStatusBar gui) (guiContextId gui)
         Gtk.statusbarPush (guiStatusBar gui) (guiContextId gui)
             (guiTranslate gui MsgDrawHelp)

         let input = Input
                     { inputGUI        = gui
                     , inputCurModeMgr = ref
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
