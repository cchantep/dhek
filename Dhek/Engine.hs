{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Dhek.Engine where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad ((<=<))
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.RWS.Strict
import Control.Monad.State

import Data.Array (Array, array)
import Data.Foldable (traverse_)
import Data.IORef
import Data.Maybe (fromJust)
import Data.Traversable (traverse)

import Graphics.UI.Gtk ( AttrOp( (:=) ))
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import qualified Graphics.UI.Gtk.Poppler.Page     as Poppler

import Dhek.Types

type EngineCallback a = a -> RWST EngineEnv () EngineState IO ()

data Engine = Engine
    { _engineState       :: IORef EngineState
    , _engineEnv         :: IORef EngineEnv
    , _enginePdfSel   :: EngineCallback PdfSelection
    , _engineJsonLoad :: EngineCallback JsonLoad
    , _engineNextPage  :: EngineCallback NextPage
    , _enginePrevPage :: EngineCallback PrevPage
    , _engineNextZoom :: EngineCallback NextZoom
    , _enginePrevZoom :: EngineCallback PrevZoom
    , _engineRemoveRect :: EngineCallback RemoveRect
    , _engineRectSelected :: EngineCallback RectSelected }

data EngineInternal = EngineInternal
    { engineRef :: IORef Viewer
    }

data PdfSelection = PdfSelection
    { pdfURI      :: !String
    , pdfFilename :: !String }

data JsonLoad = JsonLoad { jsonURI :: !String }

data NextPage = NextPage
data PrevPage = PrevPage

data NextZoom = NextZoom
data PrevZoom = PrevZoom

data RemoveRect = RemoveRect Rect

data RectSelected = RectSelected Rect

data EngineState = EngineState
    { _engineCurPage   :: {-# UNPACK #-} !Int
    , _engineCurZoom   :: {-# UNPACK #-} !Int
    , _engineCollision :: !Bool
    , _engineEvent     :: !(Maybe BoardEvent)
    }

data EngineEnv = EngineEnv
    { _engineX        :: {-# UNPACK #-} !Double
    , _engineY        :: {-# UNPACK #-} !Double
    , _enginePrevX    :: {-# UNPACK #-} !Double
    , _enginePrevY    :: {-# UNPACK #-} !Double
    , _engineOverRect :: !(Maybe Rect)
    , _engineOverArea :: !(Maybe Area)
    , _engineSelected :: !(Maybe Rect)
    }

gtkEngineNew :: IO Engine
gtkEngineNew = do
    eRef <- newIORef envNew
    sRef <- newIORef sNew
    return $ Engine
        sRef
        eRef
        (\_ -> return ())
        (\_ -> return ())
        (\_ -> return ())
        (\_ -> return ())
        (\_ -> return ())
        (\_ -> return ())
        (\_ -> return ())
        (\_ -> return ())
  where
    envNew =
        let neg1 = negate 1 in
        EngineEnv neg1 neg1 neg1 neg1 Nothing Nothing Nothing

    sNew = EngineState 1 3 True Nothing

engineStart :: Engine -> IO ()
engineStart eng = do
    Gtk.initGUI
    iRef <- newIORef (error "impossible situation")
    window  <- Gtk.windowNew
    vbox    <- Gtk.vBoxNew False 10
    fdialog <- createPdfChooserDialog window
    jdialog <- createJsonChooserDialog window
    mbar    <- Gtk.menuBarNew
    malign  <- Gtk.alignmentNew 0 0 1 0
    fitem   <- Gtk.menuItemNewWithLabel "File"
    oitem   <- Gtk.menuItemNewWithLabel "Open PDF"
    iitem   <- Gtk.menuItemNewWithLabel "Load mappings"
    sitem   <- Gtk.menuItemNewWithLabel "Save mappings"
    prev    <- Gtk.buttonNewWithLabel "Previous"
    next    <- Gtk.buttonNewWithLabel "Next"
    minus   <- Gtk.buttonNewWithLabel "-"
    plus    <- Gtk.buttonNewWithLabel "+"
    rem     <- Gtk.buttonNewWithLabel "Remove"
    store   <- Gtk.listStoreNew ([] :: [Rect])
    treeV   <- Gtk.treeViewNewWithModel store
    sel     <- Gtk.treeViewGetSelection treeV
    fmenu   <- Gtk.menuNew
    Gtk.menuShellAppend fmenu oitem
    Gtk.menuShellAppend fmenu iitem
    Gtk.menuShellAppend fmenu sitem
    Gtk.menuItemSetSubmenu fitem fmenu
    Gtk.menuShellAppend mbar fitem
    Gtk.containerAdd malign mbar
    Gtk.widgetSetSensitive iitem False
    Gtk.widgetSetSensitive sitem False
    Gtk.boxPackStart vbox malign Gtk.PackNatural 0
    let envRef   = _engineEnv eng
        stateRef = _engineState eng
        fPdf     = _enginePdfSel eng
        jsonLF   = _engineJsonLoad eng
        nextPF   = _engineNextPage eng
        prevPF   = _enginePrevPage eng
        minusPF  = _enginePrevZoom eng
        plusPF   = _engineNextZoom eng
        remF     = _engineRemoveRect eng
        selF     = _engineRectSelected eng

    Gtk.on oitem Gtk.menuItemActivate $ do
        resp <- Gtk.dialogRun fdialog
        Gtk.widgetHide fdialog
        case resp of
            Gtk.ResponseCancel -> return ()
            Gtk.ResponseOk -> do
                uriOpt  <- Gtk.fileChooserGetURI fdialog
                nameOpt <- Gtk.fileChooserGetFilename fdialog
                iOpt    <- traverse makeInternal uriOpt
                env     <- readIORef envRef
                s       <- readIORef stateRef
                traverse (writeIORef iRef) iOpt
                let evtOpt = PdfSelection <$> uriOpt <*> nameOpt
                (s', _) <- execRWST (traverse_ fPdf evtOpt) env s
                writeIORef stateRef s'
                Gtk.widgetSetSensitive oitem False
    Gtk.on iitem Gtk.menuItemActivate $ do
        resp <- Gtk.dialogRun jdialog
        Gtk.widgetHide jdialog
        case resp of
            Gtk.ResponseCancel -> return ()
            Gtk.ResponseOk -> do
                fOpt    <- Gtk.fileChooserGetFilename jdialog
                env     <- readIORef envRef
                s       <- readIORef stateRef
                (s', _) <- execRWST (traverse_ (jsonLF . JsonLoad) fOpt) env s
                writeIORef stateRef s'
    Gtk.on prev Gtk.buttonActivated $ do
        env     <- readIORef envRef
        s       <- readIORef stateRef
        (s', _) <- execRWST (prevPF PrevPage) env s
        writeIORef stateRef s'
    Gtk.on next Gtk.buttonActivated $ do
        env     <- readIORef envRef
        s       <- readIORef stateRef
        (s', _) <- execRWST (nextPF NextPage) env s
        writeIORef stateRef s'
    Gtk.on minus Gtk.buttonActivated $ do
        env     <- readIORef envRef
        s       <- readIORef stateRef
        (s', _) <- execRWST (minusPF PrevZoom) env s
        writeIORef stateRef s'
    Gtk.on plus Gtk.buttonActivated $ do
        env     <- readIORef envRef
        s       <- readIORef stateRef
        (s', _) <- execRWST (plusPF NextZoom) env s
        writeIORef stateRef s'
    Gtk.on rem Gtk.buttonActivated $ do
        env     <- readIORef envRef
        s       <- readIORef stateRef
        (s', _) <- execRWST (remF (RemoveRect $ error "not now")) env s
        writeIORef stateRef s'
    Gtk.on sel Gtk.treeSelectionSelectionChanged $ do
        env     <- readIORef envRef
        s       <- readIORef stateRef
        sOpt    <- Gtk.treeSelectionGetSelected sel
        (s', _) <- execRWST
                   (traverse_ (selF <=< liftIO . retrieveRect store) sOpt) env s
        writeIORef stateRef s'
    Gtk.containerAdd window vbox
    Gtk.set window windowParams
    Gtk.onDestroy window Gtk.mainQuit
    Gtk.widgetShowAll window
    Gtk.mainGUI
  where
    makeInternal uri = do
        v    <- loadPdf uri
        vRef <- newIORef v
        return $ EngineInternal vRef
    retrieveRect store it =
        let idx = Gtk.listStoreIterToIndex it in
        fmap RectSelected (Gtk.listStoreGetValue store idx)


createPdfChooserDialog :: Gtk.Window -> IO Gtk.FileChooserDialog
createPdfChooserDialog win = do
  ch   <- Gtk.fileChooserDialogNew title
          (Just win) Gtk.FileChooserActionOpen responses
  filt <- Gtk.fileFilterNew
  Gtk.fileFilterAddPattern filt "*.pdf"
  Gtk.fileFilterSetName filt "PDF File"
  Gtk.fileChooserAddFilter ch filt
  return ch
    where
      responses = [("Open", Gtk.ResponseOk)
                  ,("Cancel", Gtk.ResponseCancel)]
      title = Just "Open a PDF file"

createJsonChooserDialog :: Gtk.Window -> IO Gtk.FileChooserDialog
createJsonChooserDialog win = do
  ch   <- Gtk.fileChooserDialogNew title (Just win)
          Gtk.FileChooserActionSave responses
  filt <- Gtk.fileFilterNew
  Gtk.fileFilterAddPattern filt "*.json"
  Gtk.fileFilterSetName filt "Json File"
  Gtk.fileChooserAddFilter ch filt
  Gtk.fileChooserSetDoOverwriteConfirmation ch True
  return ch
    where
      responses = [("Save", Gtk.ResponseOk)
                  ,("Cancel", Gtk.ResponseCancel)]
      title = Just "Open a Json file"

windowParams :: [Gtk.AttrOp Gtk.Window]
windowParams =
    [Gtk.windowTitle          := "Dhek PDF Viewer"
    ,Gtk.windowDefaultWidth   := 800
    ,Gtk.windowDefaultHeight  := 600
    ,Gtk.containerBorderWidth := 10]

loadPdf :: FilePath -> IO Viewer
loadPdf path = do
  doc   <- fmap fromJust (Poppler.documentNewFromFile path Nothing)
  nb    <- Poppler.documentGetNPages doc
  pages <- loadPages doc
  return (Viewer doc pages 1 nb 100 3 1.0 (boardsNew nb))

loadPages :: Poppler.Document -> IO (Array Int PageItem)
loadPages doc = do
    nb <- Poppler.documentGetNPages doc
    fmap (array (1,nb)) (traverse go [1..nb])
  where
    go i = do
        page  <- Poppler.documentGetPage doc (i-1)
        (w,h) <- Poppler.pageGetSize page
        return (i, PageItem page w h)
