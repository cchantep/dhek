module Main where

import Control.Arrow ((&&&))
import Control.Applicative (WrappedMonad(..))
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Data.Monoid (First(..))
import Data.Foldable (traverse_, foldMap)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Poppler.Document
import Graphics.UI.Gtk.Poppler.Annotation
import Graphics.UI.Gtk.Poppler.Page

data Viewer =
            Viewer { viewerArea           :: DrawingArea
                   , viewerDocument       :: Document
                   , viewerScrolledWindow :: ScrolledWindow
                   , viewerCurrentPage    :: Int
                   , viewerPageCount      :: Int
                   , viewerZoom           :: Double
                   , viewerBaseWidth      :: Int
                   , viewerRects          :: [Rect]
                   , viewerSelectedRect   :: Maybe Rect
                   , viewerThickness      :: Double
                   , viewerSelection      :: Maybe Rect }

type PageHanler = Page -> IO ()

data Rect = Rect { rectX :: Double
                 , rectY :: Double
                 , rectH :: Double
                 , rectW :: Double } deriving (Eq, Show)

data Field = Field { fieldRect  :: Rect
                   , fieldType  :: String
                   , fieldValue :: String }

main :: IO ()
main = do
  initGUI
  window   <- windowNew
  hbox     <- hBoxNew False 10
  pageNav  <- vBoxNew False 0
  pageInfo <- vBoxNew False 0
  treeV    <- createPageInfoPanel
  align    <- createControlPanel pageNav
  boxPackStart pageNav align PackNatural 10
  containerAdd pageInfo treeV
  containerAdd hbox pageNav
  containerAdd hbox pageInfo
  containerAdd window hbox
  set window windowParams
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

createViewerVar :: IO (TVar (Maybe Viewer))
createViewerVar = newTVarIO Nothing

createPageInfoPanel :: IO TreeView
createPageInfoPanel = treeViewNew

createControlPanel :: VBox -> IO Alignment
createControlPanel vbox = do
  vVar   <- createViewerVar
  align  <- alignmentNew 1 0 0 0
  bbox   <- hButtonBoxNew
  fchb   <- createFileChooserButton
  label  <- labelNew Nothing
  spinB  <- spinButtonNewWithRange 0 0 1
  scale  <- hScaleNewWithRange 1 200 1
  (prev, nxt) <- createNavButtons spinB vVar
  button <- createViewButton vbox fchb nxt prev label spinB scale vVar
  widgetSetSensitive spinB False
  widgetSetSensitive scale False
  widgetSetSensitive prev False
  widgetSetSensitive nxt False
  rangeSetValue scale 100
  onValueSpinned spinB (pageBrowserChanged spinB vVar)
  scale `on` valueChanged $ pageZoomChanged scale vVar
  containerAdd align bbox
  containerAdd bbox prev
  containerAdd bbox spinB
  containerAdd bbox label
  containerAdd bbox nxt
  containerAdd bbox fchb
  containerAdd bbox scale
  containerAdd bbox button
  set bbox [buttonBoxLayoutStyle := ButtonboxStart]
  prev `on` buttonActivated $ updateEntry spinB vVar
  nxt `on` buttonActivated $ updateEntry spinB vVar
  return align

    where
      updateEntry spinB vVar = do
        (Just v) <- readTVarIO vVar
        spinButtonSetValue spinB
                          (fromIntegral $ succ $ viewerCurrentPage v)

pageBrowserChanged :: SpinButton -> TVar (Maybe Viewer) -> IO ()
pageBrowserChanged spinB viewerVar = do
  value <- spinButtonGetValueAsInt spinB
  join $ atomically $ action value
    where
      action page =
        readTVar viewerVar >>= \vOpt ->
          let nothingToDo = return (return ())
              go v =
                let area      = viewerArea v
                    cur       = viewerCurrentPage v
                    newViewer = v{ viewerCurrentPage=cur+1 } in
                writeTVar viewerVar (Just newViewer) >>= \_ ->
                  return (widgetQueueDraw area) in
          maybe nothingToDo go vOpt

pageZoomChanged :: HScale -> TVar (Maybe Viewer) -> IO ()
pageZoomChanged scale viewerVar = do
  value <- rangeGetValue scale
  join $ atomically $ action (value / 100)
    where
      action value = do
        (Just v) <- readTVar viewerVar
        let area = viewerArea v
        writeTVar viewerVar (Just v{viewerZoom = value})
        return (widgetQueueDraw area)

windowParams :: [AttrOp Window]
windowParams =
  [windowTitle          := "Dhek PDF Viewer"
  ,windowDefaultWidth   := 800
  ,windowDefaultHeight  := 600
  ,containerBorderWidth := 10]

createFileChooserButton :: IO FileChooserButton
createFileChooserButton = do
  fcb  <- fileChooserButtonNew "Select PDF File" FileChooserActionOpen
  filt <- fileFilterNew
  fileFilterAddPattern filt "*.pdf"
  fileFilterSetName filt "Pdf File"
  fileChooserAddFilter fcb  filt
  return fcb

createNavButtons :: SpinButton -> TVar (Maybe Viewer) -> IO (Button, Button)
createNavButtons spinB viewerVar = do
  predB <- buttonNewWithLabel "Previous"
  nextB <- buttonNewWithLabel "Next"
  predB `on` buttonActivated $ onPred predB nextB
  nextB `on` buttonActivated $ onNext predB nextB
  onValueSpinned spinB (updateButtonsState spinB predB nextB)
  return (predB, nextB)
    where
      onPred predB nextB =
        let inactivePrev curPage _ = curPage - 2 < 0
            action = common inactivePrev pred predB nextB
            stm = maybe nothingToDo action =<< readTVar viewerVar in
        join $ atomically stm

      onNext predB nextB =
        let inactiveNext curPage nb = curPage + 2 > (nb - 1)
            action = common inactiveNext succ nextB predB
            stm = maybe nothingToDo action =<< readTVar viewerVar in
        join $ atomically stm

      common k upd self target v =
        let curr   = viewerCurrentPage v
            nb     = viewerPageCount v
            area   = viewerArea v
            action = do
              if k curr nb
                 then widgetSetSensitive self False
                 else widgetSetSensitive target True
              widgetQueueDraw area in
        fmap (const action) $
        writeTVar viewerVar (Just (v{viewerCurrentPage=upd curr}))

      updateButtonsState spinB predB nextB =
        do (Just v) <- readTVarIO viewerVar
           value <- spinButtonGetValueAsInt spinB
           let nb = viewerPageCount v
           when (value - 1 < 1) (widgetSetSensitive predB False)
           when (value + 1 > nb) (widgetSetSensitive nextB False)
           when (value - 1 >= 1) (widgetSetSensitive predB True)
           when (value + 1 <= nb) (widgetSetSensitive nextB True)

      nothingToDo = return (return ())

createViewButton :: VBox
                 -> FileChooserButton
                 -> Button
                 -> Button
                 -> Label
                 -> SpinButton
                 -> HScale
                 -> TVar (Maybe Viewer)
                 -> IO Button
createViewButton vbox chooser nxt prev label spinB scale viewerVar = do
  button <- buttonNewWithLabel "View"
  onClicked button (go button)
  return button

  where
    go button = do
      select <- fileChooserGetFilename chooser
      maybe (print "(No Selection)") (makeView button) select

    makeView button filepath = do
      updateViewer filepath viewerVar
      join $ atomically $ action button
      widgetShowAll vbox

    action button =
      readTVar viewerVar >>= \(Just v) ->
        return $ do
          let pagesStr   = show $ viewerPageCount v
              charLength = length pagesStr
              doc        = viewerDocument v
              swin       = viewerScrolledWindow v
              cur        = viewerCurrentPage v
              nPages     = viewerPageCount v
          labelSetText label ("/ " ++ pagesStr)
          spinButtonSetValue spinB (fromIntegral (cur + 1))
          spinButtonSetRange spinB 1 (fromIntegral nPages)
          boxPackStart vbox swin PackGrow 0
          widgetSetSensitive spinB True
          widgetSetSensitive chooser False
          widgetSetSensitive button False
          widgetSetSensitive scale True
          widgetSetSensitive prev False
          if nPages == 1
             then widgetSetSensitive nxt False
             else widgetSetSensitive nxt True

createTable :: IO Table
createTable = tableNew 2 2 False

testRecs :: [Rect]
testRecs = [Rect 10 50 100 50
           ,Rect 170 60 50 100]

updateViewer :: String -> TVar (Maybe Viewer) -> IO ()
updateViewer filepath var = do
  area <- drawingAreaNew
  doc  <- liftM (\(Just x) -> x) (documentNewFromFile ("file://" ++ filepath) Nothing)
  swin <- scrolledWindowNew Nothing Nothing
  scrolledWindowAddWithViewport swin area
  scrolledWindowSetPolicy swin PolicyAutomatic PolicyAutomatic
  nPages <- documentGetNPages doc
  widgetAddEvents area [PointerMotionMask]
  let viewer = Viewer area  doc swin 0 nPages 1 760 testRecs Nothing 5.0 Nothing
  atomically $ writeTVar var (Just viewer)
  area `on` exposeEvent $ tryEvent $ viewerDraw var
  area `on` motionNotifyEvent $ tryEvent $ onMove
  area `on` buttonPressEvent $ tryEvent $ onPress
  void $ area `on` buttonReleaseEvent $ tryEvent $ onRelease

    where
      onPress = do
        (x, y) <- eventCoordinates
        ratio <- getPageRatio
        liftIO $ putStrLn ("Start in " ++ show (x,y))
        liftIO $ atomically $ modifyTVar var (fmap (\v -> v{viewerSelection= Just (Rect (x/ratio) (y/ratio) 0 0)}))

      onRelease = do
         (x, y) <- eventCoordinates
         liftIO $ putStrLn ("End in " ++ show (x,y))
         liftIO $ atomically $ modifyTVar var (fmap (\v -> v{viewerSelection=Nothing}))

      onMove = do
        ratio <- getPageRatio
        rectDetection var ratio
        updateSelection var ratio
        area <- liftIO $ fmap (fmap viewerArea) (readTVarIO var)
        maybe (return ()) (liftIO . widgetQueueDraw) area

      getPageRatio :: EventM a Double
      getPageRatio = liftIO $ fmap (\(_,r,_,_) -> r) (getPageAndSize var)

rectDetection :: TVar (Maybe Viewer) -> Double -> EventM EMotion ()
rectDetection var ratio = do
  info <- liftIO $ atomically stm
  maybe (return ()) go info
  where
    stm =
      let f v =
            let rects = viewerRects v
                thick = viewerThickness v in
            (rects, thick / 2) in
      fmap (fmap f) (readTVar var)

    overRect thick x y r@(Rect rX rY height width) =
      let adjustX = (rX + width  + thick) * ratio
          adjustY = (rY + height + thick) * ratio in

      x >= ((rX - thick) * ratio) && x <= adjustX &&
      y >= ((rY - thick) * ratio) && y <= adjustY

    go (rects, thick) =
      eventCoordinates >>= \(x,y) ->
        let f r | overRect thick x y r = First (Just r)
                | otherwise            = First Nothing
            (First res) = foldMap f rects
            action = modifyTVar var (fmap (\v -> v{viewerSelectedRect=res})) in
        liftIO $ atomically action

updateSelection :: TVar (Maybe Viewer) -> Double -> EventM EMotion ()
updateSelection var ratio = do
  sel <- getSel
  maybe (return ()) go sel
  where
    getSel = liftIO $ fmap (viewerSelection =<<) (readTVarIO var)

    go (Rect x0 y0 _ _) =
      eventCoordinates >>= \(x,y) ->
        let newHeight = (y/ratio) - y0
            newWidth  = (x/ratio) - x0
            f v = v{viewerSelection = Just (Rect x0 y0 newHeight newWidth)} in
        liftIO $ atomically $ modifyTVar var (fmap f)

getPageAndSize :: TVar (Maybe Viewer) -> IO (Page, Double, Double, Double)
getPageAndSize var =
  readTVarIO var >>= \(Just v) ->
    let doc   = viewerDocument v
        cur   = viewerCurrentPage v
        baseW = viewerBaseWidth v
        zoom  = viewerZoom v in
    do page <- documentGetPage doc (cur - 1)
       (width, height) <- pageGetSize page
       let rWidth = (fromIntegral baseW) * zoom
           ratio  = rWidth / width
       return (page, ratio, rWidth, ratio * height)

viewerDraw :: TVar (Maybe Viewer) -> EventM EExpose ()
viewerDraw = liftIO . go
  where
    go var = do
      (page, ratio, width, height) <- getPageAndSize var
      (Just (th, (area, (rs, (sel, rectSel))))) <- fmap (fmap selector) (readTVarIO var)
      frame <- widgetGetDrawWindow area
      widgetSetSizeRequest area (truncate width) (truncate height)
      renderWithDrawable frame (setSourceRGB 1.0 1.0 1.0 >>
                                scale ratio ratio        >>
                                pageRender page          >>
                                --pushGroup                >>
                                drawRects th sel rs >>
                                drawingSel rectSel) -- >>
                                --popGroupToSource)

    selector = viewerThickness    &&&
               viewerArea         &&&
               viewerRects        &&&
               viewerSelectedRect &&&
               viewerSelection

    drawRects th sel = unwrapMonad . traverse_ (WrapMonad . drawing th sel)

    drawing :: Double -> Maybe Rect -> Rect -> Render ()
    drawing th sel r@(Rect x y h w) =
      let step (Just s)
            | s == r    = setSourceRGB 1.0 0 0
            | otherwise = setSourceRGB 0 0 1.0
          step _ = setSourceRGB 0 0 1.0 in
      do step sel
         setLineWidth th
         rectangle x y w h
         closePath
         stroke

    drawingSel = unwrapMonad . traverse_ (WrapMonad . go)
      where
        go r@(Rect x y h w) =
          do  setSourceRGB 0 1.0 0
              setLineWidth 1
              rectangle x y w h
              closePath
              stroke
