{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Mode.Selection
--
--------------------------------------------------------------------------------
module Dhek.Mode.Selection (selectionModeManager) where

--------------------------------------------------------------------------------
import Prelude hiding (mapM_)
import Control.Applicative
import Data.Foldable (for_, foldMap, mapM_, traverse_)
import Data.List (sortBy)
import Data.Maybe (isJust)
import Data.Traversable

--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.RWS hiding (mapM_)
import           Control.Monad.State (evalState)
import           Control.Monad.Trans
import qualified Graphics.Rendering.Cairo     as Cairo
import qualified Graphics.UI.Gtk              as Gtk
import qualified Graphics.UI.Gtk.Poppler.Page as Poppler
import           System.FilePath (joinPath, dropFileName)
import           System.Environment.Executable (getExecutablePath)

--------------------------------------------------------------------------------
import Dhek.Engine.Type
import Dhek.Geometry
import Dhek.GUI
import Dhek.GUI.Action
import Dhek.Instr
import Dhek.Mode.Common.Draw
import Dhek.Types

--------------------------------------------------------------------------------
data Input
    = Input
      { inputGUI  :: GUI
      , inputTop  :: Gtk.Button
      , inputDist :: Gtk.Button
      }

--------------------------------------------------------------------------------
newtype SelectionMode a
    = SelectionMode (RWST Input () EngineState IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Input
             , MonadState EngineState
             , MonadIO
             )

--------------------------------------------------------------------------------
instance ModeMonad SelectionMode where
    mMove opts = do
        engineDrawState.drawOverRect .= getOverRect opts
        sOpt <- use $ engineDrawState.drawSelection

        for_ sOpt $ \s -> do
            let pos = drawPointer opts
            engineDrawState.drawSelection ?= updateDrawSelection pos s

    mPress opts = do
        let (x,y)        = drawPointer opts
            newSelection = rectNew x y 0 0

        engineDrawState.drawSelection ?= newSelection
        engineDrawState.drawMultiSel  .= []

        gui <- asks inputGUI
        liftIO $ do
            gtkSetCursor (Just Gtk.Crosshair) gui
            Gtk.treeSelectionUnselectAll $ guiRectTreeSelection gui

    mRelease = do
        input <- ask
        sOpt  <- use $ engineDrawState.drawSelection
        engineDrawState.drawSelection .= Nothing

        for_ (fmap normalize sOpt) $ \r -> do
            rs <- engineStateGetRects

            -- get rectangles located in selection area
            let crs          = foldMap (collectSelected r) rs
                hasSelection = not $ null crs
                atLeast3     = length crs >= 3
            -- if no area is selected, we disable 'top' and 'distribute' button
            liftIO $ do
                Gtk.widgetSetSensitive (inputTop input) hasSelection
                Gtk.widgetSetSensitive (inputDist input) atLeast3

            for_ crs $ \cr ->
                liftIO $ gtkSelectRect cr $ inputGUI input

            engineDrawState.drawMultiSel .= crs
            liftIO $ gtkSetCursor Nothing $ inputGUI input

      where
        collectSelected r c
            | rectInArea c r = [c]
            | otherwise      = []


    mDrawing page ratio = do
        gui <- asks inputGUI
        ds  <- use $ engineDrawState
        gds <- use $ engineBoards.boardsGuides
        gd  <- use $ engineBoards.boardsCurGuide
        rs  <- engineStateGetRects

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
                -- Paint page background in white
                Cairo.setSourceRGB 1.0 1.0 1.0
                Cairo.rectangle 0 0 fw fh
                Cairo.fill

                Cairo.scale ratio ratio
                Poppler.pageRender (pagePtr page)
                mapM_ (drawGuide fw fh) gds
                mapM_ (drawGuide fw fh) gd
                Cairo.closePath
                Cairo.stroke

                -- We consider every rectangle as regular one (e.g not selected)
                traverse_ (drawRect fw fh regularColor Line) rs

                -- Draw drawing selection rectangle
                for_ (ds ^. drawSelection) $ \r ->
                    drawRect fw fh selectionColor Dash r

                for_ (ds ^. drawMultiSel) $ \r ->
                    drawRect fw fh selectedColor Line r
      where
        regularColor   = rgbBlue
        selectedColor  = rgbRed
        selectionColor = rgbGreen

--------------------------------------------------------------------------------
-- | Called when 'Top' button, located in mode's toolbar, is clicked
topButtonActivated :: EngineCtx m => GUI -> m ()
topButtonActivated gui = do
    rs <- use $ engineDrawState.drawMultiSel
    case rs of
        []   -> return ()
        x:xs -> do
            let toppest  = foldr cmp x xs
                topY     = toppest ^. rectY
                toppedRs = fmap (updY topY) rs

            -- TODO: better mulitiselection management
            engineStateSetRects toppedRs
            engineDrawState.drawMultiSel .= toppedRs
            engineEventStack %= (UpdateRectPos:)
            liftIO $ Gtk.widgetQueueDraw $ guiDrawingArea gui
  where
    cmp r1 r2
        | r1 ^. rectY < r2 ^. rectY = r1
        | otherwise                 = r2

    updY y r = r & rectY .~ y

--------------------------------------------------------------------------------
-- | Called when 'Distribute' button, located in mode's toolbar, is clicked
distButtonActivated :: EngineCtx m => GUI -> m ()
distButtonActivated gui = do
    rs <- use $ engineDrawState.drawMultiSel
    case rs of
        []   -> return ()
        x:xs -> do
            let sorted = sortBy sortF rs
                _AN = fromIntegral $ length rs -- number of selected area
                _AW = foldr sumWidthF 0 rs -- selected areas width summed
                _L  = head sorted -- most left rectangle
                _R  = last sorted -- most right rectangle
                _D  = _R ^. rectX - _L ^. rectX + _R ^. rectWidth -- _L and _R
                                                                  -- distance
                _S  = (_D - _AW) / (_AN - 1) -- space between rectangles
                action = for (tail sorted) $ \r -> do
                    _P <- get
                    let _I = _P ^. rectX + _P ^. rectWidth
                        r' = r & rectX .~ _I + _S
                    put r'
                    return r'
                spaced = _L:(evalState action _L) -- homogeneous-spaced
                                                  -- rectangle list

            -- TODO: better mulitiselection management
            engineStateSetRects spaced
            engineDrawState.drawMultiSel .= spaced
            engineEventStack %= (UpdateRectPos:)
            liftIO $ Gtk.widgetQueueDraw $ guiDrawingArea gui

            return ()
  where
    sumWidthF :: Rect -> Double -> Double
    sumWidthF r s = s + realToFrac (r ^. rectWidth)

    sortF a b = compare (a ^. rectX) (b ^. rectX)

--------------------------------------------------------------------------------
runSelection :: GUI
             -> Gtk.Button
             -> Gtk.Button
             -> SelectionMode a
             -> EngineState
             -> IO EngineState
runSelection gui btop bdist (SelectionMode m)  s = do
    (s', _) <- execRWST m input s
    return s'
  where
    input
        = Input
          { inputGUI  = gui
          , inputTop  = btop
          , inputDist = bdist
          }

--------------------------------------------------------------------------------
selectionMode :: GUI -> Gtk.Button -> Gtk.Button -> Mode
selectionMode gui btop bdist = Mode (runSelection gui btop bdist . runM)

--------------------------------------------------------------------------------
selectionModeManager :: ((forall m. EngineCtx m => m ()) -> IO ())
                     -> GUI
                     -> IO ModeManager
selectionModeManager handler gui = do
    Gtk.treeSelectionSetMode (guiRectTreeSelection gui) Gtk.SelectionMultiple
    execPath <- getExecutablePath
    let resDir = joinPath [dropFileName execPath, "resources"]

    -- Top button
    btop <- Gtk.buttonNew
    bimg <- Gtk.imageNewFromFile $ joinPath [resDir, "top.png"]
    Gtk.buttonSetImage btop bimg
    Gtk.containerAdd toolbar btop
    Gtk.widgetSetSensitive btop False
    cid <- Gtk.on btop Gtk.buttonActivated $ handler $ topButtonActivated gui

    -- Distribute button
    bdist <- Gtk.buttonNew
    dimg  <- Gtk.imageNewFromFile $ joinPath [resDir, "distribute.png"]
    Gtk.buttonSetImage bdist dimg
    Gtk.containerAdd toolbar bdist
    Gtk.widgetSetSensitive bdist False
    did <- Gtk.on bdist Gtk.buttonActivated $ handler $ distButtonActivated gui


    Gtk.widgetShowAll btop
    Gtk.widgetShowAll bdist

    return $ ModeManager (selectionMode gui btop bdist) $ liftIO $ do
        Gtk.signalDisconnect cid
        Gtk.signalDisconnect did
        Gtk.widgetDestroy btop
        Gtk.widgetDestroy bdist
        Gtk.treeSelectionSetMode (guiRectTreeSelection gui)
            Gtk.SelectionSingle
  where
    toolbar = guiModeToolbar gui

--------------------------------------------------------------------------------
-- | Utilities
--------------------------------------------------------------------------------
rectInArea :: Rect -- target
           -> Rect -- area
           -> Bool
rectInArea t a = tx      >= ax      &&
                 ty      >= ay      &&
                 (tx+tw) <= (ax+aw) &&
                 (ty+th) <= (ay+ah)
  where
    tx = t ^. rectX
    ty = t ^. rectY
    tw = t ^. rectWidth
    th = t ^. rectHeight

    ax = a ^. rectX
    ay = a ^. rectY
    aw = a ^. rectWidth
    ah = a ^. rectHeight
