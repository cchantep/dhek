{-# LANGUAGE BangPatterns #-}
module Dhek.ViewerRef
    ( ViewerRef
    , viewerRef
    , viewerRefWindow
    , viewerDraw
    , viewerAppendStore
    , viewerGetTreeSelection
    , viewerInsertRect
    , viewerSelectRect
    , viewerSetRect
    , viewerGetEvent
    , viewerSetEvent
    , viewerModifyEvent
    , viewerGetSelection
    , viewerSetSelection
    , viewerModifySelection
    , viewerClearSelection
    , viewerGetOvered
    , viewerSetOvered
    , viewerGetPageItem
    , viewerGetSelected
    , viewerSetSelected
    , viewerGetRatio
    , viewerGetPageRects
    , viewerGetOveredRect
    , viewerGetOveredArea
    , viewerLookupIter
    ) where

import Control.Lens
import qualified Control.Monad.State as State
import Control.Monad.Trans (liftIO)

import Data.Array (Array, array, (!))
import Data.Functor ((<$))
import qualified Data.IntMap as I
import Data.IORef
import Data.Monoid (First(..))
import Data.Foldable (foldMap, traverse_)
import Data.Traversable (traverse)

import qualified Graphics.UI.Gtk as Gtk

import Dhek.Types

data ViewerRef = ViewerRef
    { _viewerRef          :: IORef Viewer
    , _viewerRefArea      :: Gtk.DrawingArea
    , _viewerRefStore     :: Gtk.ListStore Rect
    , _viewerRefSelection :: Gtk.TreeSelection
    , viewerRefWindow     :: Gtk.Window }

viewerRef :: IORef Viewer
          -> Gtk.DrawingArea
          -> Gtk.ListStore Rect
          -> Gtk.TreeSelection
          -> Gtk.Window
          -> ViewerRef
viewerRef = ViewerRef

zoomValues :: Array Int Double
zoomValues = array (0, 10) values
  where
    values = [(0,  0.125) -- 12.5%
             ,(1,  0.25)  -- 25%
             ,(2,  0.5)   -- 50%
             ,(3,  1.0)   -- 100%
             ,(4,  2.0)   -- 200%
             ,(5,  3.0)   -- 300%
             ,(6,  4.0)   -- 400%
             ,(7,  5.0)   -- 500%
             ,(8,  6.0)   -- 600%
             ,(9,  7.0)   -- 700%
             ,(10, 8.0)]  -- 800%

viewerDraw :: ViewerRef -> IO ()
viewerDraw = Gtk.widgetQueueDraw . _viewerRefArea

viewerGetTreeSelection :: ViewerRef -> IO (Maybe (Gtk.TreeIter, Rect))
viewerGetTreeSelection v = traverse go =<< Gtk.treeSelectionGetSelected sel
  where
    sel   = _viewerRefSelection v
    store = _viewerRefStore v
    go it =
        let idx = Gtk.listStoreIterToIndex it in
        fmap (\r -> (it, r)) (Gtk.listStoreGetValue store idx)

viewerInsertRect :: ViewerRef -> Rect -> IO ()
viewerInsertRect v r = do
    i <- viewerAppendStore v r
    withRectStoreIter store (Gtk.treeSelectionSelectIter sel) i
  where
    store = _viewerRefStore v
    sel   = _viewerRefSelection v

viewerSelectRect :: ViewerRef -> Rect -> IO ()
viewerSelectRect v r = do
    iOpt <- lookupStoreIter p store
    traverse_ (Gtk.treeSelectionSelectIter sel) iOpt
  where
    store = _viewerRefStore v
    sel   = _viewerRefSelection v
    p x   = (x ^. rectId) == (r ^. rectId)

viewerSetRect :: ViewerRef -> Rect -> IO ()
viewerSetRect v r = do
    writeIORef ref . State.execState go =<< readIORef ref
    iOpt <- lookupStoreIter ((== (r ^. rectId)) . _rectId) store
    traverse_ upd iOpt
  where
    ref   = _viewerRef v
    store = _viewerRefStore v
    go    = do
        page <- use viewerCurrentPage
        let id = r ^. rectId
        viewerBoards.boardsMap.at page.traverse.boardRects.at id ?= r
        viewerBoards.boardsEvent    .= None
        viewerBoards.boardsSelected ?= r
    upd it =
        let idx = Gtk.listStoreIterToIndex it in
        Gtk.listStoreSetValue store idx r

viewerAppendStore :: ViewerRef -> Rect -> IO Int
viewerAppendStore v r = do
    v  <- readIORef ref
    r' <- State.evalStateT go v
    Gtk.listStoreAppend store r'
  where
    ref    = _viewerRef v
    store  = _viewerRefStore v
    go     = do
        viewerBoards.boardsState += 1
        page <- use viewerCurrentPage
        id   <- use $ viewerBoards.boardsState
        let r' = r & rectId .~ id & rectName %~ (++  show id)
        viewerBoards.boardsMap.at page.traverse.boardRects.at id ?= r'
        viewerBoards.boardsSelection .= Nothing
        v <- State.get
        liftIO $ writeIORef ref v
        return r'

viewerGetEvent :: ViewerRef -> IO BoardEvent
viewerGetEvent v = fmap go (readIORef ref)
  where
    ref  = _viewerRef v
    go v = v ^. viewerBoards.boardsEvent

viewerSetEvent :: ViewerRef -> BoardEvent -> IO ()
viewerSetEvent v e = modifyIORef' ref (State.execState go)
  where
    ref = _viewerRef v

    go = do
        viewerBoards.boardsEvent .= e
        traverse_ upd (eventGetRect e)

    upd r = do
        page <- use viewerCurrentPage
        let id = r ^. rectId
        viewerBoards.boardsMap.at page.traverse.boardRects.at id .= Nothing

viewerModifyEvent :: ViewerRef -> (BoardEvent -> BoardEvent) -> IO ()
viewerModifyEvent v k = do
    e <- viewerGetEvent v
    let !e' = case e of
                None -> e
                _    -> k e
    viewerSetEvent v e'

viewerGetSelection :: ViewerRef -> IO (Maybe Rect)
viewerGetSelection v = fmap go (readIORef ref)
  where
    ref  = _viewerRef v
    go v = v ^. viewerBoards.boardsSelection

viewerSetSelection :: ViewerRef -> Rect -> IO ()
viewerSetSelection v r = modifyIORef' ref go
  where
    ref  = _viewerRef v
    go v = v & viewerBoards.boardsSelection ?~ r

viewerModifySelection :: ViewerRef -> (Rect -> Rect) -> IO ()
viewerModifySelection v k = do
    sOpt <- viewerGetSelection v
    traverse_ (viewerSetSelection v) (fmap k sOpt)

viewerClearSelection :: ViewerRef -> IO ()
viewerClearSelection v = modifyIORef' ref go
  where
    ref  = _viewerRef v
    go v = v & viewerBoards.boardsSelection .~ Nothing

viewerGetOvered :: ViewerRef -> IO (Maybe Rect)
viewerGetOvered = fmap go . readIORef . _viewerRef
  where
    go v = v ^. viewerBoards.boardsOvered

viewerSetOvered :: ViewerRef -> Maybe Rect -> IO ()
viewerSetOvered v rOpt = modifyIORef' ref go
  where
    ref  = _viewerRef v
    go v = v & viewerBoards.boardsOvered .~ rOpt

viewerGetPageItem :: ViewerRef -> IO PageItem
viewerGetPageItem = fmap go . readIORef . _viewerRef
  where
    go v =
        let idx   = v ^. viewerCurrentPage
            pages = v ^. viewerPages in
        pages ! idx

viewerGetSelected :: ViewerRef -> IO (Maybe Rect)
viewerGetSelected = fmap go . readIORef . _viewerRef
  where
    go v = v ^. viewerBoards.boardsSelected

viewerSetSelected :: ViewerRef -> Maybe Rect -> IO ()
viewerSetSelected v rOpt = modifyIORef' ref go
  where
    ref  = _viewerRef v
    go v = v & viewerBoards.boardsSelected .~ rOpt

viewerGetRatio :: ViewerRef -> IO Double
viewerGetRatio v = fmap go (readIORef ref)
  where
    ref  = _viewerRef v
    go v =
        let pId   = v ^. viewerCurrentPage
            pZ    = v ^. viewerZoom
            pages = v ^. viewerPages
            baseW = fromIntegral (v ^. viewerBaseWidth)
            page  = pages ! pId
            zoom  = zoomValues ! pZ
            w     = pageWidth page
        in (baseW * zoom) / w

viewerGetPageRects :: ViewerRef -> IO [Rect]
viewerGetPageRects = fmap go . readIORef . _viewerRef
  where
    go v =
        let pId = v ^. viewerCurrentPage in
        v ^. viewerBoards.boardsMap.at pId.traverse.boardRects.to I.elems

viewerGetOveredRect :: ViewerRef -> Double -> Double -> IO (Maybe Rect)
viewerGetOveredRect v x y = do
    rs <- viewerGetPageRects v
    fmap (go rs) (readIORef ref)
  where
    ref = _viewerRef v

    go rs v =
        let (First oOpt) = foldMap (First . overed) rs in oOpt

    overed r
        | isOver 1.0 x y r = Just r
        | otherwise        = Nothing

viewerGetOveredArea :: ViewerRef
                    -> Double
                    -> Double
                    -> Rect
                    -> IO (Maybe Area)
viewerGetOveredArea v x y r = return . go =<< viewerGetRatio v
  where
    go ratio =
        let (First aOpt) =
                foldMap (First . overed ratio) (enumFrom TOP_LEFT) in
        aOpt

    overed ratio a
        | isOver 1.0 x y (rectArea (5/ratio) r a) = Just a
        | otherwise                               = Nothing

withRectStoreIter :: Gtk.ListStore Rect
                  -> (Gtk.TreeIter -> IO r)
                  -> Int
                  -> IO ()
withRectStoreIter store k i =
    Gtk.treeModelForeach store $ \iter ->
        if Gtk.listStoreIterToIndex iter == i
        then True <$ k iter
        else return False

viewerLookupIter :: ViewerRef -> (Rect -> Bool) -> IO (Maybe Rect)
viewerLookupIter v p = fmap go (viewerGetPageRects v)
  where
    ref = _viewerRef v
    go  = getFirst . foldMap (First . search)

    search r
        | p r       = Just r
        | otherwise = Nothing

lookupStoreIter :: (a -> Bool) -> Gtk.ListStore a -> IO (Maybe Gtk.TreeIter)
lookupStoreIter pred store = Gtk.treeModelGetIterFirst store >>= go
  where
    go (Just it) = do
        a <- Gtk.listStoreGetValue store (Gtk.listStoreIterToIndex it)
        if pred a
            then return (Just it)
            else Gtk.treeModelIterNext store it >>= go
    go _ = return Nothing
