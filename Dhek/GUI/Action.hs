--------------------------------------------------------------------------------
-- |
-- Module : Dhek.GUI.Action
--
--------------------------------------------------------------------------------
module Dhek.GUI.Action
    ( gtkUnselect
    , gtkSelectRect
    , gtkAddRect
    , gtkSetCursor
    ) where

--------------------------------------------------------------------------------
import Data.Foldable (for_, traverse_)
import Data.Traversable (traverse)

--------------------------------------------------------------------------------
import           Control.Lens
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
import Dhek.GUI
import Dhek.Types

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
