{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.GUI.Action
--
--------------------------------------------------------------------------------
module Dhek.GUI.Action
    ( DhekClosingChoice(..)
    , DhekCursor(..)
    , DhekCursorType(..)
    , gtkUnselect
    , gtkSelectRect
    , gtkAddRect
    , gtkSetDhekCursor
    , gtkIncrPage
    , gtkDecrPage
    , gtkIncrZoom
    , gtkDecrZoom
    , gtkRemoveRect
    , gtkLookupEntryText
    , gtkGetTreeSelection
    , gtkGetTreeAllSelection
    , gtkShowError
    , gtkSelectJsonFile
    , gtkOpenJsonFile
    , gtkSetRects
    , gtkSetOverlapActive
    , gtkSetMagneticActive
    , gtkSetValuePropVisible
    , gtkShowConfirm
    , gtkSetIndexPropVisible
    , gtkGetIndexPropValue
    , gtkClearSelection
    , gtkShowWarning
    ) where

--------------------------------------------------------------------------------
import Data.Char (isSpace)
import Data.Foldable (for_, traverse_)
import Data.Maybe (fromMaybe)
import Data.Traversable (for, sequenceA)
import Foreign.Ptr

--------------------------------------------------------------------------------
import           Control.Lens
import qualified Data.Text       as T
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
import           Dhek.GUI
import           Dhek.I18N
import qualified Dhek.Resources as Resources
import           Dhek.Types

--------------------------------------------------------------------------------
data DhekClosingChoice
    = DhekSave
    | DhekDontSave
    | DhekCancel

--------------------------------------------------------------------------------
data DhekCursor
    = GTKCursor Gtk.CursorType
    | DhekCursor DhekCursorType

--------------------------------------------------------------------------------
data DhekCursorType
    = CursorDup
    | CursorSelection
    | CursorSelectionUpdate

--------------------------------------------------------------------------------
-- | When a rectangle is unselected
gtkUnselect :: GUI -> IO ()
gtkUnselect gui = do
    Gtk.widgetSetSensitive (guiTypeCombo gui) False
    Gtk.widgetSetSensitive (guiNameEntry gui) False
    Gtk.widgetSetSensitive (guiRemoveButton gui) False
    Gtk.widgetSetSensitive (guiApplyButton gui) False
    Gtk.entrySetText (guiNameEntry gui) ("" :: String)
    Gtk.comboBoxSetActive (guiTypeCombo gui) (- 1)
    gtkSetValuePropVisible False gui
    gtkSetIndexPropVisible False gui

--------------------------------------------------------------------------------
-- | When a rectangle is selected
gtkSelectRect :: Rect -> GUI -> IO ()
gtkSelectRect r gui = do
    iOpt <- lookupStoreIter (sameRectId r) $ guiRectStore gui
    for_ iOpt $ \it -> do
        let idx = Gtk.listStoreIterToIndex it

        Gtk.listStoreSetValue (guiRectStore gui) idx r
        Gtk.treeSelectionSelectIter (guiRectTreeSelection gui) it
        Gtk.entrySetText (guiNameEntry gui) name

        case r ^. rectValue of
            Nothing -> Gtk.entrySetText (guiValueEntry gui) ("" :: String)
            Just v  -> Gtk.entrySetText (guiValueEntry gui) v

        case r ^. rectIndex of
            Nothing -> Gtk.spinButtonSetValue (guiIndexSpin gui) 0
            Just i  -> Gtk.spinButtonSetValue (guiIndexSpin gui)
                       (realToFrac i)

        tOpt <- lookupStoreIter ((== typ)) (guiTypeStore gui)

        traverse_ (Gtk.comboBoxSetActiveIter $ guiTypeCombo gui) tOpt
        Gtk.widgetSetSensitive (guiRemoveButton gui) True
        Gtk.widgetSetSensitive (guiApplyButton gui) True
        Gtk.widgetSetSensitive (guiTypeCombo gui) True
        Gtk.widgetSetSensitive (guiNameEntry gui) True

  where
    typ  = r ^. rectType
    name = r ^. rectName

--------------------------------------------------------------------------------
-- | When a rectangle is created
gtkAddRect :: Rect -> GUI -> IO ()
gtkAddRect r gui = do
    _    <-  Gtk.listStoreAppend (guiRectStore gui) r
    iOpt <- lookupStoreIter (sameRectId r) (guiRectStore gui)
    traverse_ (Gtk.treeSelectionSelectIter $ guiRectTreeSelection gui) iOpt

--------------------------------------------------------------------------------
gtkSetDhekCursor :: GUI -> Maybe DhekCursor -> IO ()
gtkSetDhekCursor gui odc
    = case odc of
        Nothing -> gtkSetImageCursor gui Resources.mouseNormal
        Just dc ->
            case dc of
                GTKCursor t  -> gtkSetGtkCursor gui t
                DhekCursor d -> gtkSetImageCursor gui (dhekCursorImage d)

--------------------------------------------------------------------------------
gtkSetGtkCursor :: GUI -> Gtk.CursorType -> IO ()
gtkSetGtkCursor gui t = do
    c     <- Gtk.cursorNew t
    frame <- Gtk.widgetGetDrawWindow $ guiDrawingArea gui
    Gtk.drawWindowSetCursor frame (Just c)

--------------------------------------------------------------------------------
dhekCursorImage :: DhekCursorType -> Ptr Gtk.InlineImage
dhekCursorImage CursorDup             = Resources.mouseDup
dhekCursorImage CursorSelection       = Resources.mouseSelection
dhekCursorImage CursorSelectionUpdate = Resources.mouseUpdate

--------------------------------------------------------------------------------
gtkSetImageCursor :: GUI -> Ptr Gtk.InlineImage -> IO ()
gtkSetImageCursor gui img
    = do odis <- Gtk.displayGetDefault
         for_ odis $ \display ->
             do pix   <- Gtk.pixbufNewFromInline img
                cur   <- Gtk.cursorNewFromPixbuf display pix 1 1
                frame <- Gtk.widgetGetDrawWindow $ guiDrawingArea gui
                Gtk.drawWindowSetCursor frame (Just cur)

--------------------------------------------------------------------------------
gtkIncrPage :: Int -> Int -> [Rect] -> GUI -> IO ()
gtkIncrPage ncur nb rects gui = do
    Gtk.widgetSetSensitive (guiPrevButton gui) True
    Gtk.widgetSetSensitive (guiNextButton gui) (ncur < nb)
    Gtk.listStoreClear $ guiRectStore gui
    traverse_ (Gtk.listStoreAppend $ guiRectStore gui) rects

--------------------------------------------------------------------------------
gtkDecrPage :: Int -> Int -> [Rect] -> GUI -> IO ()
gtkDecrPage ncur nmin rects gui = do
     Gtk.widgetSetSensitive (guiPrevButton gui) (ncur > nmin)
     Gtk.widgetSetSensitive (guiNextButton gui) True
     Gtk.listStoreClear (guiRectStore gui)
     traverse_ (Gtk.listStoreAppend $ guiRectStore gui) rects

--------------------------------------------------------------------------------
gtkIncrZoom :: Int -> Int -> GUI -> IO ()
gtkIncrZoom ncur nmax gui = do
    Gtk.widgetSetSensitive (guiZoomOutButton gui) True
    Gtk.widgetSetSensitive (guiZoomInButton gui) (ncur < nmax)

--------------------------------------------------------------------------------
gtkDecrZoom :: Int -> Int -> GUI -> IO ()
gtkDecrZoom ncur nmin gui = do
    Gtk.widgetSetSensitive (guiZoomOutButton gui) (ncur > nmin)
    Gtk.widgetSetSensitive (guiZoomInButton gui) True

--------------------------------------------------------------------------------
gtkRemoveRect :: Rect -> GUI -> IO ()
gtkRemoveRect r gui = do
    iOpt <- lookupStoreIter (sameRectId r) (guiRectStore gui)
    for_ iOpt $ \it ->
        let idx = Gtk.listStoreIterToIndex it in
        Gtk.listStoreRemove (guiRectStore gui) idx

--------------------------------------------------------------------------------
gtkLookupEntryText :: Gtk.Entry -> IO (Maybe T.Text)
gtkLookupEntryText entry = do
    txt <- Gtk.entryGetText entry
    let txt1 = trimString txt
        r    = if T.null txt1 then Nothing else Just txt1
    return r

--------------------------------------------------------------------------------
gtkShowError :: String -> GUI -> IO ()
gtkShowError e gui = do
    m <- Gtk.messageDialogNew (Just $ guiWindow gui)
         [Gtk.DialogModal] Gtk.MessageError Gtk.ButtonsOk e
    _ <- Gtk.dialogRun m
    Gtk.widgetHide m

--------------------------------------------------------------------------------
gtkGetTreeSelection :: GUI -> IO (Maybe Rect)
gtkGetTreeSelection gui = do
    iOpt <- Gtk.treeSelectionGetSelected $ guiRectTreeSelection gui
    for iOpt $ \it ->
        let idx = Gtk.listStoreIterToIndex it in
        Gtk.listStoreGetValue (guiRectStore gui) idx

--------------------------------------------------------------------------------
gtkGetTreeAllSelection :: GUI -> IO [Rect]
gtkGetTreeAllSelection gui
    = do tps  <- Gtk.treeSelectionGetSelectedRows $ guiRectTreeSelection gui
         itsO <- traverse (Gtk.treeModelGetIter $ guiRectStore gui) tps
         let its = fromMaybe [] $ sequenceA itsO
         for its $ \it ->
             let idx = Gtk.listStoreIterToIndex it in
             Gtk.listStoreGetValue (guiRectStore gui) idx

--------------------------------------------------------------------------------
gtkSelectJsonFile :: GUI -> IO (Maybe FilePath)
gtkSelectJsonFile gui = do
    resp <- Gtk.dialogRun $ guiJsonSaveDialog gui
    Gtk.widgetHide $ guiJsonSaveDialog gui
    case resp of
        Gtk.ResponseOk -> Gtk.fileChooserGetFilename $ guiJsonSaveDialog gui
        _              -> return Nothing

--------------------------------------------------------------------------------
gtkOpenJsonFile :: GUI -> IO (Maybe FilePath)
gtkOpenJsonFile gui = do
    resp <- Gtk.dialogRun $ guiJsonOpenDialog gui
    Gtk.widgetHide $ guiJsonOpenDialog gui
    case resp of
        Gtk.ResponseOk -> Gtk.fileChooserGetFilename $ guiJsonOpenDialog gui
        _              -> return Nothing
--------------------------------------------------------------------------------
gtkSetRects :: [Rect] -> GUI -> IO ()
gtkSetRects rects gui = do
    Gtk.listStoreClear $ guiRectStore gui
    traverse_ (Gtk.listStoreAppend $ guiRectStore gui) rects

--------------------------------------------------------------------------------
gtkSetOverlapActive :: GUI -> Bool -> IO ()
gtkSetOverlapActive gui b
    = Gtk.checkMenuItemSetActive (guiOverlapMenuItem gui) b

--------------------------------------------------------------------------------
gtkSetMagneticActive :: GUI -> Bool -> IO ()
gtkSetMagneticActive gui b
    = Gtk.checkMenuItemSetActive (guiMagneticForceMenuItem gui) b

--------------------------------------------------------------------------------
gtkSetValuePropVisible :: Bool -> GUI -> IO ()
gtkSetValuePropVisible b gui
    | b         = do
        Gtk.widgetSetChildVisible (guiValueEntryAlign gui) True
        Gtk.widgetSetChildVisible (guiValueEntry gui) True
        Gtk.widgetShowAll $ guiValueEntryAlign gui
        Gtk.widgetShowAll $ guiValueEntry gui
    | otherwise = do
        Gtk.widgetHideAll $ guiValueEntryAlign gui
        Gtk.widgetHideAll $ guiValueEntry gui

--------------------------------------------------------------------------------
gtkSetIndexPropVisible :: Bool -> GUI -> IO ()
gtkSetIndexPropVisible visible gui
    | visible = do
        Gtk.widgetSetChildVisible (guiIndexAlign gui) True
        Gtk.widgetSetChildVisible (guiIndexSpin gui) True
        Gtk.widgetShowAll $ guiIndexAlign gui
        Gtk.widgetShowAll $ guiIndexSpin gui
    | otherwise = do
        Gtk.widgetHideAll $ guiIndexAlign gui
        Gtk.widgetHideAll $ guiIndexSpin gui

--------------------------------------------------------------------------------
gtkShowConfirm :: GUI -> String -> IO DhekClosingChoice
gtkShowConfirm gui msg = do
    m <- Gtk.messageDialogNew (Just $ guiWindow gui)
         [Gtk.DialogModal] Gtk.MessageWarning Gtk.ButtonsNone msg
    _ <- Gtk.dialogAddButton m (guiTranslate gui MsgSave) $ Gtk.ResponseUser 1
    _ <- Gtk.dialogAddButton m (guiTranslate gui MsgDontSave) $ Gtk.ResponseUser 2
    _ <- Gtk.dialogAddButton m (guiTranslate gui MsgCancel) $ Gtk.ResponseUser 3
    resp <- Gtk.dialogRun m
    Gtk.widgetHide m
    case resp of
        Gtk.ResponseUser 1 -> return DhekSave
        Gtk.ResponseUser 2 -> return DhekDontSave
        Gtk.ResponseUser 3 -> return DhekCancel
        _                  -> return DhekCancel

--------------------------------------------------------------------------------
gtkGetIndexPropValue :: GUI -> IO (Maybe Int)
gtkGetIndexPropValue gui = do
    opt      <- Gtk.comboBoxGetActiveText $ guiTypeCombo gui
    idxvalue <- Gtk.spinButtonGetValueAsInt $ guiIndexSpin gui
    return $ do
        typ <- opt
        if typ == "textcell"
            then Just idxvalue
            else Nothing

--------------------------------------------------------------------------------
gtkClearSelection :: GUI -> IO ()
gtkClearSelection gui = Gtk.treeSelectionUnselectAll $ guiRectTreeSelection gui

--------------------------------------------------------------------------------
gtkShowWarning :: GUI -> String -> IO ()
gtkShowWarning gui e = do
    m <- Gtk.messageDialogNew (Just $ guiWindow gui)
         [Gtk.DialogModal] Gtk.MessageWarning Gtk.ButtonsOk e
    _ <- Gtk.dialogRun m
    Gtk.widgetHide m

--------------------------------------------------------------------------------
-- | Utilities
--------------------------------------------------------------------------------
lookupStoreIter :: (a -> Bool) -> Gtk.ListStore a -> IO (Maybe Gtk.TreeIter)
lookupStoreIter predicate store = Gtk.treeModelGetIterFirst store >>= go
  where
    go (Just it) = do
        a <- Gtk.listStoreGetValue store (Gtk.listStoreIterToIndex it)
        if predicate a
            then return (Just it)
            else Gtk.treeModelIterNext store it >>= go
    go _ = return Nothing

--------------------------------------------------------------------------------
trimString :: T.Text -> T.Text
trimString = T.dropWhileEnd isSpace . T.dropWhile isSpace
