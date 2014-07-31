{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Mode.Duplicate
--
--------------------------------------------------------------------------------
module Dhek.Mode.Duplicate
    ( DuplicateMode
    , duplicateModeManager
    , dupStart
    , dupEnd
    , runDuplicate
    ) where

--------------------------------------------------------------------------------
import Prelude hiding (mapM_)
import Control.Applicative
import Data.Foldable (for_, mapM_, traverse_)
import Data.IORef

--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.RWS.Strict hiding (mapM_)
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
import Dhek.Types

--------------------------------------------------------------------------------
newtype DuplicateMode a
    = DuplicateMode (RWST GUI () EngineState IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader GUI
             , MonadState EngineState
             , MonadIO
             )

--------------------------------------------------------------------------------
instance ModeMonad DuplicateMode where
    mMove opts = do
        let oOpt = getOverRect opts

        eOpt <- use $ engineDrawState.drawEvent

        engineDrawState.drawOverRect .= oOpt

        -- We only handle move without caring about overlap
        for_ eOpt $ \e -> do
            let pos@(x,y) = drawPointer opts
            case e of
                Hold r ppos ->
                    engineDrawState.drawEvent ?=
                        Hold (updateHoldRect ppos pos r) (x,y)

                _ -> return ()

    mPress opts = do
        eOpt <- use $ engineDrawState.drawEvent
        case eOpt of
            Nothing         -> dupStart opts
            Just (Hold x _) -> dupEnd x

    mRelease _ = return ()

    mDrawing page ratio = do
        gui <- ask
        ds  <- use $ engineDrawState
        pid <- use $ engineCurPage
        bd  <- use $ engineBoards.boardsMap.at pid.traverse
        let guides = bd ^. boardGuides
            rects  = bd ^. boardRects.to I.elems

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

                -- We consider every rectangle as regular one (e.g not selected)
                traverse_ (drawRect fw fh regularColor Line) rects

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

    mKeyPress _ = return ()

    mKeyRelease _ = return ()

    mEnter = return ()

    mLeave = return ()

--------------------------------------------------------------------------------
dupStart :: DrawEnv -> DuplicateMode ()
dupStart opts
    = for_ (getOverRect opts) $ \r ->
          do rid <- engineDrawState.drawFreshId <+= 1
             let r2    = r & rectId .~ rid
                 (x,y) = drawPointer opts

             engineDrawState.drawEvent ?= Hold r2 (x,y)
             gui <- ask
             liftIO $ gtkSetDhekCursor gui (Just $ DhekCursor CursorDup)

--------------------------------------------------------------------------------
dupEnd :: Rect -> DuplicateMode ()
dupEnd x
    = do rid <- engineDrawState.drawFreshId <+= 1
         let r = normalize x & rectId    .~ rid
                             & rectIndex %~ (fmap (+1))
         -- Add rectangle
         gui <- ask
         engineStateSetRect r
         liftIO $ gtkAddRect r gui

         engineDrawState.drawEvent     .= Nothing
         engineDrawState.drawCollision .= Nothing

         liftIO $ gtkSetDhekCursor gui Nothing
         engineEventStack %= (CreateRect:)

--------------------------------------------------------------------------------
runDuplicate :: GUI -> DuplicateMode a -> EngineState -> IO EngineState
runDuplicate gui (DuplicateMode m) s = do
    (s', _) <- execRWST m gui s
    return s'

--------------------------------------------------------------------------------
duplicateMode :: GUI -> Mode
duplicateMode gui = Mode (runDuplicate gui . runM)

--------------------------------------------------------------------------------
duplicateModeManager :: GUI -> IO ModeManager
duplicateModeManager gui
    = do -- Display duplicate Help message
         Gtk.statusbarPop (guiStatusBar gui) (guiContextId gui)
         Gtk.statusbarPush (guiStatusBar gui) (guiContextId gui)
             (guiTranslate gui MsgDupHelp)

         return $ ModeManager (duplicateMode gui) (return ())
