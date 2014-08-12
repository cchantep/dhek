{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Resources
--
-- Application resources like compiled image assets
--------------------------------------------------------------------------------
module Dhek.Resources where

--------------------------------------------------------------------------------
import Foreign.Ptr

--------------------------------------------------------------------------------
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
foreign import ccall "&align_horizontal_center" alignHorizontalCenter :: Ptr Gtk.InlineImage
foreign import ccall "&align_horizontal_left" alignHorizontalLeft :: Ptr Gtk.InlineImage
foreign import ccall "&align_horizontal_right" alignHorizontalRight :: Ptr Gtk.InlineImage
foreign import ccall "&align_vertical_bottom" alignVerticalBottom :: Ptr Gtk.InlineImage
foreign import ccall "&align_vertical_center" alignVerticalCenter :: Ptr Gtk.InlineImage
foreign import ccall "&align_vertical_top" alignVerticalTop :: Ptr Gtk.InlineImage
foreign import ccall "&applidok" applidok :: Ptr Gtk.InlineImage
foreign import ccall "&dialog_accept" dialogAccept :: Ptr Gtk.InlineImage
foreign import ccall "&distribute" distribute :: Ptr Gtk.InlineImage
foreign import ccall "&distribute_create" distributeCreate :: Ptr Gtk.InlineImage
foreign import ccall "&distribute_vertical" distributeVertical :: Ptr Gtk.InlineImage
foreign import ccall "&draw_eraser" drawEraser :: Ptr Gtk.InlineImage
foreign import ccall "&draw_rectangle" drawRectangle :: Ptr Gtk.InlineImage
foreign import ccall "&duplicate_rectangle" duplicateRectangle :: Ptr Gtk.InlineImage
foreign import ccall "&go_next" goNext :: Ptr Gtk.InlineImage
foreign import ccall "&go_previous" goPrevious :: Ptr Gtk.InlineImage
foreign import ccall "&mouse_normal" mouseNormal :: Ptr Gtk.InlineImage
foreign import ccall "&mouse_dup" mouseDup :: Ptr Gtk.InlineImage
foreign import ccall "&mouse_selection" mouseSelection :: Ptr Gtk.InlineImage
foreign import ccall "&mouse_update" mouseUpdate :: Ptr Gtk.InlineImage
foreign import ccall "&rectangular_selection" rectangularSelection :: Ptr Gtk.InlineImage
foreign import ccall "&zoom_in" zoomIn :: Ptr Gtk.InlineImage
foreign import ccall "&zoom_out" zoomOut :: Ptr Gtk.InlineImage
