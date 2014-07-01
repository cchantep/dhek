--------------------------------------------------------------------------------
-- |
-- Module : Dhek.GUI.Action
--
--------------------------------------------------------------------------------
module Dhek.GUI.Action
    ( DhekClosingChoice(..)
    , gtkUnselect
    , gtkSelectRect
    , gtkAddRect
    , gtkSetCursor
    , gtkIncrPage
    , gtkDecrPage
    , gtkIncrZoom
    , gtkDecrZoom
    , gtkRemoveRect
    , gtkLookupEntryText
    , gtkGetTreeSelection
    , gtkShowError
    , gtkSelectJsonFile
    , gtkOpenJsonFile
    , gtkSetRects
    , gtkSetOverlapActive
    , gtkSetValuePropVisible
    , gtkShowConfirm
    , gtkSetIndexPropVisible
    , gtkGetIndexPropValue
    ) where

--------------------------------------------------------------------------------
import Data.Char (isSpace)
import Data.Foldable (for_, traverse_)
import Data.List (dropWhileEnd)
import Data.Traversable (for, traverse)

--------------------------------------------------------------------------------
import           Control.Lens
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
import Dhek.GUI
import Dhek.I18N
import Dhek.Types

--------------------------------------------------------------------------------
data DhekClosingChoice
    = DhekSave
    | DhekDontSave
    | DhekCancel

--------------------------------------------------------------------------------
-- | When a rectangle is unselected
gtkUnselect :: GUI -> IO ()
gtkUnselect gui = do
    Gtk.widgetSetSensitive (guiTypeCombo gui) False
    Gtk.widgetSetSensitive (guiNameEntry gui) False
    Gtk.widgetSetSensitive (guiRemoveButton gui) False
    Gtk.widgetSetSensitive (guiApplyButton gui) False
    Gtk.entrySetText (guiNameEntry gui) ""
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
            Nothing -> Gtk.entrySetText (guiValueEntry gui) ""
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
    Gtk.listStoreAppend (guiRectStore gui) r
    iOpt <- lookupStoreIter (sameRectId r) (guiRectStore gui)
    traverse_ (Gtk.treeSelectionSelectIter $ guiRectTreeSelection gui) iOpt

--------------------------------------------------------------------------------
gtkSetCursor :: Maybe Gtk.CursorType -> GUI -> IO ()
gtkSetCursor t gui = do
    c     <- traverse Gtk.cursorNew t
    frame <- Gtk.widgetGetDrawWindow $ guiDrawingArea gui
    Gtk.drawWindowSetCursor frame c

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
gtkLookupEntryText :: Gtk.Entry -> IO (Maybe String)
gtkLookupEntryText entry = do
    txt <- Gtk.entryGetText entry
    let txt1 = trimString txt
        r    = if null txt1 then Nothing else Just txt1
    return r

--------------------------------------------------------------------------------
gtkShowError :: Show a => a -> GUI -> IO ()
gtkShowError e gui = do
    m <- Gtk.messageDialogNew (Just $ guiWindow gui)
         [Gtk.DialogModal] Gtk.MessageError Gtk.ButtonsOk (show e)
    Gtk.dialogRun m
    Gtk.widgetHide m

--------------------------------------------------------------------------------
gtkGetTreeSelection :: GUI -> IO (Maybe Rect)
gtkGetTreeSelection gui = do
    iOpt <- Gtk.treeSelectionGetSelected $ guiRectTreeSelection gui
    for iOpt $ \it ->
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
gtkSetOverlapActive :: Bool -> GUI -> IO ()
gtkSetOverlapActive b gui =
    Gtk.checkMenuItemSetActive (guiOverlapMenuItem gui) b

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
    m    <- Gtk.messageDialogNew (Just $ guiWindow gui)
            [Gtk.DialogModal] Gtk.MessageWarning Gtk.ButtonsNone msg
    Gtk.dialogAddButton m (guiTranslate gui MsgSave) $ Gtk.ResponseUser 1
    Gtk.dialogAddButton m (guiTranslate gui MsgDontSave) $ Gtk.ResponseUser 2
    Gtk.dialogAddButton m (guiTranslate gui MsgCancel) $ Gtk.ResponseUser 3
    resp <- Gtk.dialogRun m
    Gtk.widgetHide m
    case resp of
        Gtk.ResponseUser 1 -> return DhekSave
        Gtk.ResponseUser 2 -> return DhekDontSave
        Gtk.ResponseUser 3 -> return DhekCancel

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
-- | Utilities
--------------------------------------------------------------------------------
lookupStoreIter :: (a -> Bool) -> Gtk.ListStore a -> IO (Maybe Gtk.TreeIter)
lookupStoreIter pred store = Gtk.treeModelGetIterFirst store >>= go
  where
    go (Just it) = do
        a <- Gtk.listStoreGetValue store (Gtk.listStoreIterToIndex it)
        if pred a
            then return (Just it)
            else Gtk.treeModelIterNext store it >>= go
    go _ = return Nothing

--------------------------------------------------------------------------------
trimString :: String -> String
trimString = dropWhileEnd isSpace . dropWhile isSpace
