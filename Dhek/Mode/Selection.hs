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
import Data.List (replicate, sortBy)
import Data.Traversable

--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.RWS hiding (mapM_)
import           Control.Monad.State (evalState)
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
      { inputGUI        :: GUI
      , inputTop        :: Gtk.Button
      , inputDist       :: Gtk.Button
      , inputDistCreate :: Gtk.Button
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
                cDistCreate  = canActiveDistCreate crs

            -- if no area is selected, we disable 'top' and 'distribute' button
            liftIO $ do
                Gtk.widgetSetSensitive (inputTop input) hasSelection
                Gtk.widgetSetSensitive (inputDist input) atLeast3
                Gtk.widgetSetSensitive (inputDistCreate input) cDistCreate

            for_ crs $ \cr ->
                liftIO $ gtkSelectRect cr $ inputGUI input

            engineDrawState.drawMultiSel .= crs
            liftIO $ gtkSetCursor Nothing $ inputGUI input

      where
        collectSelected r c
            | rectInArea c r = [c]
            | otherwise      = []


    mDrawing page ratio = do
        gui   <- asks inputGUI
        ds    <- use $ engineDrawState
        pid   <- use $ engineCurPage
        bd    <- use $ engineBoards.boardsMap.at pid.traverse
        rects <- engineStateGetRects
        let guides   = bd ^. boardGuides
            curGuide = ds ^. drawCurGuide

        liftIO $ do
            frame     <- Gtk.widgetGetDrawWindow $ guiDrawingArea gui
            (fw',fh') <- Gtk.drawableGetSize frame

            let width  = ratio * (pageWidth page)
                height = ratio * (pageHeight page)
                fw     = fromIntegral fw'
                fh     = fromIntegral fh'
                area   = guiDrawingArea gui

            Gtk.widgetSetSizeRequest area (truncate width) (truncate height)
            Gtk.renderWithDrawable frame $ do
                -- Paint page background in white
                Cairo.setSourceRGB 1.0 1.0 1.0
                Cairo.rectangle 0 0 fw fh
                Cairo.fill

                Cairo.scale ratio ratio
                Poppler.pageRender (pagePtr page)
                mapM_ (drawGuide fw fh) guides
                mapM_ (drawGuide fw fh) curGuide
                Cairo.closePath
                Cairo.stroke

                -- We consider every rectangle as regular one (e.g not selected)
                traverse_ (drawRect fw fh regularColor Line) rects

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
            let sorted = sortBy rectCompareX rs
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

--------------------------------------------------------------------------------
-- | Called when 'Distribute create' button, located in mode's toolbar,
--   is clicked
distCreateButtonActivated :: EngineCtx m => GUI -> m ()
distCreateButtonActivated gui
    = do sel <- use $ engineDrawState.drawMultiSel
         let (r0:r1:rn:_) = sortBy rectCompareIndex sel
             y    = r0 ^. rectY
             w    = r0 ^. rectWidth
             h    = r0 ^. rectHeight
             name = r0 ^. rectName
             s -- Horizontal space between 0 & 1
                 = r1 ^. rectX - (r0 ^. rectX + w)
             d -- Distance between 0 & N
                 = rn ^. rectX - (r1 ^. rectX + w + s)
             m -- Number of cells to be created between cell 1 & N
                 = floor (((realToFrac d) / realToFrac (w + s) :: Double))
             rn' -- N index is updated according to 'm' new rectangles
                 = rn & rectIndex ?~ m+1

             -- Create 'm' new rectangles
             loop _ _ []
                 = return ()
             loop prevRect idx (_:rest)
                 = do rid <- engineDrawState.drawFreshId <+= 1
                      let rx = prevRect ^. rectX + prevRect ^. rectWidth + s
                          r  = rectNew rx y h w & rectId    .~ rid
                                                & rectName  .~ name
                                                & rectType  .~ "textcell"
                                                & rectIndex ?~ idx
                      engineStateSetRect r
                      liftIO $ gtkAddRect r gui
                      engineDrawState.drawMultiSel %= (r:)
                      loop r (idx+1) rest

         loop r1 2 (replicate m ())
         engineStateSetRect rn'

--------------------------------------------------------------------------------
-- | Dist create button is enabled if only 3 textcells with same name property
--   are selected, and if indexes of these cells are 0, 1 and N>2.
canActiveDistCreate :: [Rect] -> Bool
canActiveDistCreate rs@(_:_:_:[])
    = let (r0:r1:rn:_) = sortBy rectCompareIndex rs
          areTextcell  = all ((== "textcell") . (^. rectType)) rs
          sameName     = all ((== r0 ^. rectName) . (^. rectName)) [r1,rn] in
      areTextcell               &&
      r0 ^. rectIndex == Just 0 &&
      r1 ^. rectIndex == Just 1 &&
      rn ^. rectIndex >  Just 1 &&
      sameName
canActiveDistCreate _
    = False

--------------------------------------------------------------------------------
runSelection :: GUI
             -> Gtk.Button
             -> Gtk.Button
             -> Gtk.Button
             -> SelectionMode a
             -> EngineState
             -> IO EngineState
runSelection gui btop bdist bdistcreate (SelectionMode m)  s = do
    (s', _) <- execRWST m input s
    return s'
  where
    input
        = Input
          { inputGUI        = gui
          , inputTop        = btop
          , inputDist       = bdist
          , inputDistCreate = bdistcreate
          }

--------------------------------------------------------------------------------
selectionMode :: GUI -> Gtk.Button -> Gtk.Button -> Gtk.Button -> Mode
selectionMode gui btop bdist bdistcreate
    = Mode (runSelection gui btop bdist bdistcreate. runM)

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
    bimg <- Gtk.imageNewFromFile $ joinPath [resDir, "align-vertical-top.png"]
    vsep <- Gtk.vSeparatorNew

    Gtk.buttonSetImage btop bimg
    Gtk.containerAdd toolbar vsep
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

    -- Distribute create button
    bdistcreate <- Gtk.buttonNew
    bdimg <- Gtk.imageNewFromFile $ joinPath [resDir, "distribute-create.png"]
    Gtk.buttonSetImage bdistcreate bdimg
    Gtk.containerAdd toolbar bdistcreate
    Gtk.widgetSetSensitive bdistcreate False
    dcid <- Gtk.on bdistcreate Gtk.buttonActivated $ handler $
            distCreateButtonActivated gui

    Gtk.widgetShowAll vsep
    Gtk.widgetShowAll btop
    Gtk.widgetShowAll bdist
    Gtk.widgetShowAll bdistcreate

    return $ ModeManager (selectionMode gui btop bdist bdistcreate) $
        do engineDrawState.drawMultiSel .= []
           liftIO $
               do Gtk.signalDisconnect cid
                  Gtk.signalDisconnect did
                  Gtk.signalDisconnect dcid
                  Gtk.widgetDestroy vsep
                  Gtk.widgetDestroy btop
                  Gtk.widgetDestroy bdist
                  Gtk.widgetDestroy bdistcreate
                  Gtk.widgetDestroy vsep
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
