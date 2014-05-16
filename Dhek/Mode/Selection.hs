{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import qualified Data.IntMap                  as I
import qualified Graphics.Rendering.Cairo     as Cairo
import qualified Graphics.UI.Gtk              as Gtk
import qualified Graphics.UI.Gtk.Poppler.Page as Poppler

--------------------------------------------------------------------------------
import Dhek.Engine.Type
import Dhek.Geometry
import Dhek.GUI
import Dhek.GUI.Action
import Dhek.Mode.Common.Draw
import Dhek.Types

--------------------------------------------------------------------------------
newtype SelectionMode a
    = SelectionMode (RWST GUI () EngineState IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader GUI
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

        gui <- ask
        liftIO $ do
            gtkSetCursor (Just Gtk.Crosshair) gui
            Gtk.treeSelectionUnselectAll $ guiRectTreeSelection gui

    mRelease = do
        gui  <- ask
        sOpt <- use $ engineDrawState.drawSelection
        engineDrawState.drawSelection .= Nothing

        for_ (fmap normalize sOpt) $ \r -> do
            pid <- use engineCurPage
            rs  <- use $
                   engineBoards.boardsMap.at pid.traverse.boardRects.to I.elems

            -- get rectangles located in selection area
            let crs = foldMap (collectSelected r) rs

            for_ crs $ \cr ->
                liftIO $ gtkSelectRect cr gui

            engineDrawState.drawMultiSel .= crs
            liftIO $ gtkSetCursor Nothing gui

      where
        collectSelected r c
            | rectInArea c r = [c]
            | otherwise      = []


    mDrawing page ratio = do
        gui <- ask
        ds  <- use $ engineDrawState
        gds <- use $ engineBoards.boardsGuides
        gd  <- use $ engineBoards.boardsCurGuide
        pid <- use $ engineCurPage
        rs  <- use $ engineBoards.boardsMap.at pid.traverse.boardRects.to I.elems

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
runSelection :: GUI -> SelectionMode a -> EngineState -> IO EngineState
runSelection gui (SelectionMode m)  s = do
    (s', _) <- execRWST m gui s
    return s'

--------------------------------------------------------------------------------
selectionMode :: GUI -> Mode
selectionMode gui = Mode (runSelection gui . runM)

--------------------------------------------------------------------------------
selectionModeManager :: GUI -> IO ModeManager
selectionModeManager gui = do
    Gtk.treeSelectionSetMode (guiRectTreeSelection gui) Gtk.SelectionMultiple
    return $ ModeManager (selectionMode gui) $ liftIO $
        Gtk.treeSelectionSetMode (guiRectTreeSelection gui)
        Gtk.SelectionSingle

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
