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
import Data.Foldable (for_, mapM_)
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
newtype NormalMode a
    = NormalMode (RWST GUI () EngineState IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader GUI
             , MonadState EngineState
             , MonadIO
             )

--------------------------------------------------------------------------------
instance ModeMonad NormalMode where
    mMove opts = do

        let oOpt = getOverRect opts
            aOpt = getOverArea opts

        eOpt <- use $ engineDrawState.drawEvent
        sOpt <- use $ engineDrawState.drawSelection

        engineDrawState.drawOverRect .= oOpt

        -- When user draws a rectangle
        for_ sOpt $ \s -> do
            let pos = drawPointer opts
            engineDrawState.drawSelection ?= updateDrawSelection pos s

        -- When user resizes or moves a rectangle
        for_ eOpt $ \e -> do
            if drawOverlap opts
                then overlapMode e
                else collisionMode e

        let cursorOpt = fmap eventCursor eOpt <|>
                        fmap areaCursor aOpt  <|>
                        Gtk.Hand1 <$ oOpt

        -- Update Gtk cursor
        gui <- ask
        liftIO $ gtkSetCursor cursorOpt gui

      where
        overlapMode e = do
            let pos@(x,y) = drawPointer opts
            case e of
                Hold r ppos ->
                    engineDrawState.drawEvent ?=
                        Hold (updateHoldRect ppos pos r) (x,y)

                Resize r (x0,y0) a ->
                    let dx = x-x0
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
                                (px',py')   = updateHoldPos d (x,y) (px,py)
                                catchUp     = diff <= 0

                            if not catchUp && collides
                                then
                                engineDrawState.drawEvent ?= Hold r1 (px',py')
                                else do
                                engineDrawState.drawCollision .= Nothing
                                let (r2, newPos) = correctRect d (x0',y0') r
                                engineDrawState.drawEvent ?= Hold r2 newPos

                _ -> overlapMode e -- Resize collision detection is
                               -- not supported yet.

    mPress opts = do

        let (x,y)        = drawPointer opts
            oOpt         = getOverRect opts
            aOpt         = getOverArea opts
            newSelection = rectNew x y 0 0

            onEvent r = do
                let rid = r ^. rectId
                    evt = maybe (Hold r (x,y)) (Resize r (x,y)) aOpt
                engineDrawState.drawEvent ?= evt

                pid <- use engineCurPage
                engineBoards.boardsMap.at pid.traverse.boardRects.at rid .=
                    Nothing

        -- if user click on a blank area we're un drawing mode otherwise we enter
        -- event mode (Resize or Hold).
        maybe (engineDrawState.drawSelection ?= newSelection) onEvent oOpt

    mRelease = do
        sOpt <- use $ engineDrawState.drawSelection
        eOpt <- use $ engineDrawState.drawEvent

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
                gui <- ask
                pid <- use engineCurPage
                engineBoards.boardsMap.at pid.traverse.boardRects.at rid ?= r2
                liftIO $ gtkAddRect r2 gui

                normalSelectRectangle r2

            engineDrawState.drawSelection .= Nothing

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

                -- Draw the entire board
                for_ rs $ \r ->
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

--------------------------------------------------------------------------------
runNormal :: GUI -> NormalMode a -> EngineState -> IO EngineState
runNormal gui (NormalMode m)  s = do
    (s', _) <- execRWST m gui s
    return s'

--------------------------------------------------------------------------------
normalMode :: GUI -> Mode
normalMode gui = Mode (runNormal gui . runM)

--------------------------------------------------------------------------------
normalModeManager :: GUI -> IO ModeManager
normalModeManager gui = return $ ModeManager (normalMode gui) (return ())

--------------------------------------------------------------------------------
normalSelectRectangle :: Rect -> NormalMode ()
normalSelectRectangle r = do
    engineDrawState.drawSelected ?= r
    gui <- ask
    liftIO $ gtkSelectRect r gui
