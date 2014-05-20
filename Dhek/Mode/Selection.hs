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
import Data.Maybe (isJust)
import Data.Traversable

--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.RWS hiding (mapM_)
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
      { inputGUI :: GUI
      , inputTop :: Gtk.Button
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
            let crs = foldMap (collectSelected r) rs

            -- if no area is selected, we disable 'top' button
            liftIO $ Gtk.widgetSetSensitive (inputTop input)
                (not $ null crs)

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
topButtonActivated :: GUI -> EngineCtx m => m ()
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
            engineEventStack %= (UpdateRectBounds:)
            liftIO $ Gtk.widgetQueueDraw $ guiDrawingArea gui
  where
    cmp r1 r2
        | r1 ^. rectY < r2 ^. rectY = r1
        | otherwise                 = r2

    updY y r = r & rectY .~ y

--------------------------------------------------------------------------------
runSelection :: GUI
             -> Gtk.Button
             -> SelectionMode a
             -> EngineState
             -> IO EngineState
runSelection gui btop (SelectionMode m)  s = do
    (s', _) <- execRWST m input s
    return s'
  where
    input
        = Input
          { inputGUI = gui
          , inputTop = btop
          }

--------------------------------------------------------------------------------
selectionMode :: GUI -> Gtk.Button -> Mode
selectionMode gui btop = Mode (runSelection gui btop . runM)

--------------------------------------------------------------------------------
selectionModeManager :: ((forall m. EngineCtx m => m ()) -> IO ())
                     -> GUI
                     -> IO ModeManager
selectionModeManager handler gui = do
    Gtk.treeSelectionSetMode (guiRectTreeSelection gui) Gtk.SelectionMultiple
    btop     <- Gtk.buttonNew
    execPath <- getExecutablePath
    let resDir = joinPath [dropFileName execPath, "resources"]
    bimg <- Gtk.imageNewFromFile $ joinPath [resDir, "top.png"]
    Gtk.buttonSetImage btop bimg
    Gtk.containerAdd toolbar btop
    Gtk.widgetSetSensitive btop False

    -- listen to button event
    cid <- Gtk.on btop Gtk.buttonActivated $ handler $ topButtonActivated gui

    Gtk.widgetShowAll btop

    return $ ModeManager (selectionMode gui btop) $ liftIO $ do
        Gtk.signalDisconnect cid
        Gtk.widgetDestroy btop
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
