{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Mode.Duplicate
--
--------------------------------------------------------------------------------
module Dhek.Mode.Duplicate
    ( ConcreteDuplicateMode
    , DuplicateMode
    , cdmCleanup
    , cdmRun
    , concreteDuplicateManager
    , duplicateModeManager
    , duplicateGetDupRect
    , duplicateGetGUI
    , dupStart
    , dupEnd
    , runDuplicate
    ) where

--------------------------------------------------------------------------------
import Prelude hiding (mapM_)
import Control.Applicative
import Data.Foldable (for_, traverse_)
import Data.IORef

--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.RWS.Strict hiding (mapM_)
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
    = DuplicateMode (RWST Input () EngineState IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Input
             , MonadState EngineState
             , MonadIO
             )

--------------------------------------------------------------------------------
data Dup = Dup Pos Rect

--------------------------------------------------------------------------------
data Input
    = Input
      { inputGUI :: GUI
      , inputDup :: IORef (Maybe Dup)
      }

--------------------------------------------------------------------------------
data ConcreteDuplicateMode
    = CDM { cdmRun :: forall a. DuplicateMode a
                   -> EngineState
                   -> IO EngineState
          , cdmCleanup :: EngineCtx m => m ()
          }

--------------------------------------------------------------------------------
instance ModeMonad DuplicateMode where
    mMove opts
        = do dupRef <- asks inputDup
             eOpt   <- liftIO $ readIORef dupRef

             let oOpt = getOverRect opts

             engineDrawState.drawOverRect .= oOpt

             -- We only handle move without caring about overlap
             for_ eOpt $ \(Dup pos r) ->
                 let newPos = drawPointer opts
                     newDup = Dup newPos (moveRect pos newPos r) in
                 liftIO $ writeIORef dupRef $ Just newDup

    mPress env
        = do dupRef <- asks inputDup
             eOpt   <- liftIO $ readIORef dupRef
             maybe (dupStart env) (\(Dup _ r) -> dupEnd r) eOpt

    mRelease _ = return ()

    mDrawing page ratio = do
        gui    <- asks inputGUI
        dupRef <- asks inputDup
        oEvR   <- liftIO $ readIORef dupRef
        pid    <- use $ engineCurPage
        bd     <- use $ engineBoards.boardsMap.at pid.traverse
        let rects  = bd ^. boardRects.to I.elems

        liftIO $ do
            frame     <- Gtk.widgetGetDrawWindow $ guiDrawingArea gui
            (fw',fh') <- Gtk.drawableGetSize frame

            let width  = ratio * (pageWidth page)
                height = ratio * (pageHeight page)
                fw     = fromIntegral fw'
                fh     = fromIntegral fh'
                eventR = fmap (\(Dup _ r) -> r) oEvR
                area   = guiDrawingArea gui

            Gtk.widgetSetSizeRequest area (truncate width) (truncate height)
            Gtk.renderWithDrawable frame $ do
                suf <- guiPdfSurface page ratio gui
                Cairo.setSourceSurface suf 0 0
                Cairo.paint

                Cairo.scale ratio ratio

                -- We consider every rectangle as regular one (e.g not selected)
                traverse_ (drawRect regularColor Line) rects

                -- Draw event rectangle
                for_ eventR $ \r -> do
                    drawRect selectedColor Line r
                    drawRectGuides fw fh rectGuideColor r
      where
        regularColor   = rgbBlue
        selectedColor  = rgbRed
        rectGuideColor = RGB 0.16 0.72 0.92

    mKeyPress _ = return ()

    mKeyRelease _ = return ()

    mEnter = return ()

    mLeave = return ()

--------------------------------------------------------------------------------
duplicateGetDupRect :: DuplicateMode (Maybe Rect)
duplicateGetDupRect
    = do dupRef <- asks inputDup
         dup    <- liftIO $ readIORef dupRef
         return $ fmap (\(Dup _ r) -> r) dup

--------------------------------------------------------------------------------
duplicateGetGUI :: DuplicateMode GUI
duplicateGetGUI = asks inputGUI

--------------------------------------------------------------------------------
dupStart :: DrawEnv -> DuplicateMode ()
dupStart opts
    = for_ (getOverRect opts) $ \r ->
          do rid    <- engineDrawState.drawFreshId <+= 1
             input  <- ask
             let r2     = r & rectId .~ rid
                 pos    = drawPointer opts
                 dupRef = inputDup input
                 gui    = inputGUI input

             liftIO $
                 do writeIORef dupRef $ Just $ Dup pos r2
                    gtkSetDhekCursor gui $ Just $ DhekCursor CursorDup

--------------------------------------------------------------------------------
dupEnd :: Rect -> DuplicateMode ()
dupEnd x
    = do rid <- engineDrawState.drawFreshId <+= 1
         let r = normalize x & rectId    .~ rid
                             & rectIndex %~ (fmap (+1))
         -- Add rectangle
         input <- ask
         let gui    = inputGUI input
             dupRef = inputDup input

         engineEventStack %= (CreateRect:)
         engineStateSetRect r

         liftIO $
             do gtkAddRect r gui
                gtkSetDhekCursor gui Nothing
                writeIORef dupRef Nothing

--------------------------------------------------------------------------------
runDuplicate :: Input -> DuplicateMode a -> EngineState -> IO EngineState
runDuplicate input (DuplicateMode m) s = do
    (s', _) <- execRWST m input s
    return s'

--------------------------------------------------------------------------------
concreteDuplicateManager :: GUI -> IO ConcreteDuplicateMode
concreteDuplicateManager gui
    = do -- Display duplicate Help message
         Gtk.statusbarPop (guiStatusBar gui) (guiContextId gui)
         _ <- Gtk.statusbarPush (guiStatusBar gui) (guiContextId gui)
              (guiTranslate gui MsgDupHelp)
         ref <- newIORef Nothing

         let input = Input
                     { inputGUI = gui
                     , inputDup = ref
                     }

         return $ CDM
                  { cdmRun     = runDuplicate input
                  , cdmCleanup = return ()
                  }

--------------------------------------------------------------------------------
concreteToManager :: ConcreteDuplicateMode -> ModeManager
concreteToManager (CDM run clean)
    = let mode = Mode (run . runM) in ModeManager mode clean

--------------------------------------------------------------------------------
duplicateModeManager :: GUI -> IO ModeManager
duplicateModeManager gui = fmap concreteToManager $ concreteDuplicateManager gui
