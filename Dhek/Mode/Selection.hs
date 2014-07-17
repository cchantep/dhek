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
import Foreign.Ptr

--------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.RWS hiding (mapM_)
import           Control.Monad.State (evalState)
import qualified Graphics.Rendering.Cairo     as Cairo
import qualified Graphics.UI.Gtk              as Gtk
import           System.FilePath (joinPath, dropFileName)
import           System.Environment.Executable (getExecutablePath)

--------------------------------------------------------------------------------
import           Dhek.Engine.Instr
import           Dhek.Engine.Type
import           Dhek.Geometry
import           Dhek.GUI
import           Dhek.GUI.Action
import           Dhek.Mode.Common.Draw
import qualified Dhek.Resources as Resources
import           Dhek.Types

--------------------------------------------------------------------------------
data Input
    = Input
      { inputGUI        :: GUI
      , inputTop        :: Gtk.ToolButton
      , inputDist       :: Gtk.ToolButton
      , inputDistCreate :: Gtk.ToolButton
      , inputRight      :: Gtk.ToolButton
      , inputBottom     :: Gtk.ToolButton
      , inputLeft       :: Gtk.ToolButton
      , inputHCenter    :: Gtk.ToolButton
      , inputVCenter    :: Gtk.ToolButton
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
            let crs         = foldMap (collectSelected r) rs
                atLeast2    = length crs >= 2
                atLeast3    = length crs >= 3
                cDistCreate = canActiveDistCreate crs

            -- if no area is selected, we disable 'top' and 'distribute' button
            liftIO $ do
                Gtk.widgetSetSensitive (inputTop input) atLeast2
                Gtk.widgetSetSensitive (inputDist input) atLeast3
                Gtk.widgetSetSensitive (inputDistCreate input) cDistCreate
                Gtk.widgetSetSensitive (inputRight input) atLeast2
                Gtk.widgetSetSensitive (inputBottom input) atLeast2
                Gtk.widgetSetSensitive (inputLeft input) atLeast2
                Gtk.widgetSetSensitive (inputHCenter input) atLeast2
                Gtk.widgetSetSensitive (inputVCenter input) atLeast2

            for_ crs $ \cr ->
                liftIO $ gtkSelectRect cr $ inputGUI input

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

        liftIO $ do
            rsSel     <- gtkGetTreeAllSelection gui
            frame     <- Gtk.widgetGetDrawWindow $ guiDrawingArea gui
            (fw',fh') <- Gtk.drawableGetSize frame

            let width  = ratio * (pageWidth page)
                height = ratio * (pageHeight page)
                fw     = fromIntegral fw'
                fh     = fromIntegral fh'
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

                -- Draw drawing selection rectangle
                for_ (ds ^. drawSelection) $ \r ->
                    drawRect fw fh selectionColor Dash r

                for_ rsSel $ \r ->
                    drawRect fw fh selectedColor Line r
      where
        regularColor   = rgbBlue
        selectedColor  = rgbRed
        selectionColor = rgbGreen
        guideColor     = RGB 0.16 0.26 0.87

--------------------------------------------------------------------------------
-- | Called when 'Top' button, located in mode's toolbar, is clicked
topButtonActivated :: EngineCtx m => GUI -> m ()
topButtonActivated gui = alignmentM gui AlignTop

--------------------------------------------------------------------------------
bottomButtonActivated :: EngineCtx m => GUI -> m ()
bottomButtonActivated gui = alignmentM gui AlignBottom

--------------------------------------------------------------------------------
rightButtonActivated :: EngineCtx m => GUI -> m ()
rightButtonActivated gui = alignmentM gui AlignRight

--------------------------------------------------------------------------------
leftButtonActivated :: EngineCtx m => GUI -> m ()
leftButtonActivated gui = alignmentM gui AlignLeft

--------------------------------------------------------------------------------
alignmentM :: EngineCtx m => GUI -> Align -> m ()
alignmentM gui align
    = do rs <- liftIO $ gtkGetTreeAllSelection gui
         let rs' = alignment align rs

         engineStateSetRects rs'
         forM_ rs' $ \r ->
             liftIO $ gtkSelectRect r gui
         engineEventStack %= (UpdateRectPos:)
         liftIO $ Gtk.widgetQueueDraw $ guiDrawingArea gui

--------------------------------------------------------------------------------
data Align
    = AlignTop
    | AlignRight
    | AlignBottom
    | AlignLeft
    | AlignHCenter
    | AlignVCenter

--------------------------------------------------------------------------------
data Bin a b = Bin a b

--------------------------------------------------------------------------------
alignment :: Align -> [Rect] -> [Rect]
alignment align rects
    | (x:xs) <- rects =
        case align of
            AlignTop     -> go id topCmp topUpd x xs
            AlignRight   -> go id rightCmp rightUpd x xs
            AlignBottom  -> go id bottomCmp bottomUpd x xs
            AlignLeft    -> go id leftCmp leftUpd x xs
            AlignHCenter -> go hcInit hcCmp hcUpd x xs
            AlignVCenter -> go vcInit vcCmp vcUpd x xs

    | otherwise = []

  where
    go initK cmpK updK r rs
        = let res = foldr cmpK (initK r) rs in
          fmap (updK res) rects

    -- Top
    topCmp r1 r2
        | r1 ^. rectY < r2 ^. rectY = r1
        | otherwise                 = r2

    topUpd toppest r
        = r & rectY .~ (toppest ^. rectY)

    --Right
    rightCmp r1 r2
        | r1 ^. rectX + r1 ^. rectWidth > r2 ^. rectX + r2 ^. rectWidth = r1
        | otherwise = r2

    rightUpd rightest r
        = r & rectX +~ delta
      where
        rmx   = r ^. rectX + r ^. rectWidth
        mx    = rightest ^. rectX + rightest ^. rectWidth
        delta = mx - rmx

    -- Bottom
    bottomCmp r1 r2
        | r1 ^. rectY + r1 ^. rectHeight > r2 ^. rectY + r2 ^. rectHeight = r1
        | otherwise = r2

    bottomUpd bottomest r
        = r & rectY +~ delta
      where
        rmy   = r ^. rectY + r ^. rectHeight
        my    = bottomest ^. rectY + bottomest ^. rectHeight
        delta = my - rmy

    -- Left
    leftCmp r1 r2
        | r1 ^. rectX < r2 ^. rectX = r1
        | otherwise                 = r2

    leftUpd leftest r
        = r & rectX .~ (leftest ^. rectX)

    -- Horizontal Center
    lenX r = r ^. rectX + r ^. rectWidth

    hcInit r =
        Bin (r ^. rectX) (lenX r)

    hcCmp r s@(Bin leftest rightest)
        = let newLeftest = if r ^. rectX < leftest
                           then r ^. rectX
                           else leftest

              newRightest = if lenX r > rightest
                            then lenX r
                            else rightest in

          Bin newLeftest newRightest

    hcUpd (Bin leftest rightest) r
        = r & rectX .~ center - (r ^. rectWidth / 2)
      where
        len    = rightest - leftest
        center = leftest + len / 2

    -- Vertical Center
    lenY r = r ^. rectY + r ^. rectHeight

    vcInit r =
        Bin (r ^. rectY) (lenY r)

    vcCmp r s@(Bin toppest bottomest)
        = let newToppest = if r ^. rectY < toppest
                           then r ^. rectY
                           else toppest

              newBottomest = if lenY r > bottomest
                             then lenY r
                             else bottomest in

          Bin newToppest newBottomest

    vcUpd (Bin toppest bottomest) r
        = r & rectY .~ center - (r ^. rectHeight / 2)
      where
        len    = bottomest - toppest
        center = toppest + len / 2

--------------------------------------------------------------------------------
-- | Called when 'Distribute' button, located in mode's toolbar, is clicked
distButtonActivated :: EngineCtx m => GUI -> m ()
distButtonActivated gui = do
    rs <- liftIO $ gtkGetTreeAllSelection gui
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
            engineStateSetRects spaced
            forM_ spaced $ \r ->
                liftIO $ gtkSelectRect r gui
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
    = do sel <- liftIO $ gtkGetTreeAllSelection gui
         let (r0:r1:rn:_) = sortBy rectCompareIndex sel
             y    = r0 ^. rectY
             w    = r0 ^. rectWidth
             h    = r0 ^. rectHeight
             name = r0 ^. rectName
             s -- Horizontal space between 0 & 1
                 = r1 ^. rectX - (r0 ^. rectX + w)
             d -- Distance between left of 1 & right of N
                 = rn ^. rectX - (r1 ^. rectX + w + s)
             m -- Number of cells to be created between cell 1 & N
                 = floor (((realToFrac d) / realToFrac (w + s) :: Double))
             rn' -- N index is updated according to 'm' new rectangles
                 = rn & rectIndex ?~ m+2 -- 'm' rect after 2nd rect(1) + 1

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
                      loop r (idx+1) rest

         loop r1 2 (replicate m ())
         engineStateSetRect rn'

--------------------------------------------------------------------------------
hCenterActivated :: EngineCtx m => GUI -> m ()
hCenterActivated gui = alignmentM gui AlignHCenter

--------------------------------------------------------------------------------
vCenterActivated :: EngineCtx m => GUI -> m ()
vCenterActivated gui = alignmentM gui AlignVCenter

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
runSelection :: Input -> SelectionMode a -> EngineState -> IO EngineState
runSelection input (SelectionMode m) s
    = do (s', _) <- execRWST m input s
         return s'

--------------------------------------------------------------------------------
selectionMode :: Input -> Mode
selectionMode input = Mode (runSelection input . runM)

--------------------------------------------------------------------------------
selectionModeManager :: ((forall m. EngineCtx m => m ()) -> IO ())
                     -> GUI
                     -> IO ModeManager
selectionModeManager handler gui = do
    Gtk.treeSelectionSetMode (guiRectTreeSelection gui) Gtk.SelectionMultiple

    vsep1 <- Gtk.separatorToolItemNew
    Gtk.separatorToolItemSetDraw vsep1 False
    Gtk.toolbarInsert toolbar vsep1 (-1)
    Gtk.widgetShowAll vsep1

    -- Top button
    btop <- createToolbarButton gui Resources.alignVerticalTop
    cid <- Gtk.onToolButtonClicked btop $ handler $ topButtonActivated gui

    -- Vertical Center button
    bvcenter <- createToolbarButton gui Resources.alignVerticalCenter
    bvid     <- Gtk.onToolButtonClicked bvcenter $ handler $
                vCenterActivated gui

    -- Bottom button
    bbottom <- createToolbarButton gui Resources.alignVerticalBottom
    bbid    <- Gtk.onToolButtonClicked bbottom $ handler $
               bottomButtonActivated gui

    -- Left button
    bleft <- createToolbarButton gui Resources.alignHorizontalLeft
    lid   <- Gtk.onToolButtonClicked bleft $ handler $
             leftButtonActivated gui

    -- Horizontal Center button
    bhcenter <- createToolbarButton gui Resources.alignHorizontalCenter
    bhid     <- Gtk.onToolButtonClicked bhcenter $ handler $
                hCenterActivated gui

    -- Right button
    bright <- createToolbarButton gui Resources.alignHorizontalRight
    rid    <- Gtk.onToolButtonClicked bright $ handler $
              rightButtonActivated gui

    vsep2 <- Gtk.separatorToolItemNew
    Gtk.separatorToolItemSetDraw vsep2 False
    Gtk.toolbarInsert toolbar vsep2 (-1)
    Gtk.widgetShowAll vsep2

    -- Distribute button
    bdist <- createToolbarButton gui Resources.distribute
    did   <- Gtk.onToolButtonClicked bdist $ handler $ distButtonActivated gui

    -- Distribute create button
    bdistcreate <- createToolbarButton gui Resources.distributeCreate
    dcid        <- Gtk.onToolButtonClicked bdistcreate $ handler $
                   distCreateButtonActivated gui

    let input = Input
                { inputGUI        = gui
                , inputTop        = btop
                , inputDist       = bdist
                , inputDistCreate = bdistcreate
                , inputRight      = bright
                , inputBottom     = bbottom
                , inputLeft       = bleft
                , inputHCenter    = bhcenter
                , inputVCenter    = bvcenter
                }

    return $ ModeManager (selectionMode input) $
        liftIO $ do Gtk.signalDisconnect cid
                    Gtk.signalDisconnect did
                    Gtk.signalDisconnect dcid
                    Gtk.signalDisconnect rid
                    Gtk.signalDisconnect bbid
                    Gtk.signalDisconnect lid
                    Gtk.signalDisconnect bhid
                    Gtk.signalDisconnect bvid

                    Gtk.containerForeach toolbar $ \w ->
                        do i <- Gtk.toolbarGetItemIndex toolbar $
                                Gtk.castToToolItem w
                           if i == 0
                               then return ()
                               else Gtk.containerRemove toolbar w

                    Gtk.treeSelectionSetMode (guiRectTreeSelection gui)
                        Gtk.SelectionSingle
  where
    toolbar = guiModeToolbar gui

--------------------------------------------------------------------------------
createToolbarButton :: GUI -> Ptr Gtk.InlineImage -> IO Gtk.ToolButton
createToolbarButton gui img
    = do img <- loadImage img
         b   <- Gtk.toolButtonNew (Just img) Nothing
         Gtk.toolbarInsert (guiModeToolbar gui) b (-1)
         Gtk.widgetShowAll b
         Gtk.widgetSetSensitive b False
         return b

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
