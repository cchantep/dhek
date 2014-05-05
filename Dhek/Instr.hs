{-# LANGUAGE ExistentialQuantification #-}
module Dhek.Instr where

import Control.Monad.Free
import Control.Monad.Free.Church

import Data.Foldable (for_, traverse_)

import Dhek.Types
import Dhek.Free

import Graphics.Rendering.Cairo (Render)
import Graphics.UI.Gtk.Gdk.Cursor (CursorType)

type DhekProgram a = Free DhekInstr a

data DhekEntry = PropEntry
               | ValueEntry

data DhekCombo = PropCombo

data DhekOption = Overlap

data DhekToggle = DrawToggle
                | MultiSelToggle

data DhekInstr a = GetSelected (Maybe Rect -> a)
                 | SetSelected !(Maybe Rect) a
                 | GetRectangles ([Rect] -> a)
                 | GetCurPage (Int -> a)
                 | GetPageCount (Int -> a)
                 | IncrPage a
                 | IncrZoom a
                 | DecrPage a
                 | DecrZoom a
                 | RemoveRect !Rect a
                 | GetEntryText !DhekEntry (Maybe String -> a)
                 | GetComboText !DhekCombo (Maybe String -> a)
                 | UnselectRect a
                 | Draw a
                 | SetTitle !String a
                 | GetFilename (String -> a)
                 | ShowError !String a
                 | forall b. PerformIO !(IO b) (b -> a)
                 | GetTreeSelection (Maybe Rect -> a)
                 | NewGuide GuideType a
                 | UpdateGuide a
                 | AddGuide a
                 | GetCurGuide (Maybe Guide -> a)
                 | GetGuides ([Guide] -> a)
                 | SelectJsonFile (Maybe String -> a)
                 | GetAllRects ([(Int, [Rect])] -> a)
                 | SetRects [(Int, [Rect])] a
                 | OpenJsonFile (Maybe String -> a)
                 | Active DhekOption Bool a
                 | IsActive DhekOption (Bool -> a)
                 | SetValuePropVisible Bool a
                 | IsToggleActive DhekToggle (Bool -> a)
                 | SetToggleActive DhekToggle Bool a

instance Functor DhekInstr where
    fmap f (GetSelected k)      = GetSelected (f . k)
    fmap f (SetSelected r a)    = SetSelected r (f a)
    fmap f (GetRectangles k)    = GetRectangles (f . k)
    fmap f (GetCurPage k)       = GetCurPage (f . k)
    fmap f (GetPageCount k)     = GetPageCount (f . k)
    fmap f (IncrPage a)         = IncrPage (f a)
    fmap f (DecrPage a)         = DecrPage (f a)
    fmap f (IncrZoom a)         = IncrZoom (f a)
    fmap f (DecrZoom a)         = DecrZoom (f a)
    fmap f (RemoveRect r a)     = RemoveRect r (f a)
    fmap f (GetEntryText e k)   = GetEntryText e (f . k)
    fmap f (GetComboText e k)   = GetComboText e (f . k)
    fmap f (UnselectRect a)     = UnselectRect (f a)
    fmap f (Draw a)             = Draw (f a)
    fmap f (SetTitle t a)       = SetTitle t (f a)
    fmap f (GetFilename k)      = GetFilename (f . k)
    fmap f (ShowError e a)      = ShowError e (f a)
    fmap f (PerformIO o k)      = PerformIO o (f . k)
    fmap f (GetTreeSelection k) = GetTreeSelection (f . k)
    fmap f (NewGuide g a)       = NewGuide g (f a)
    fmap f (UpdateGuide a)      = UpdateGuide (f a)
    fmap f (AddGuide a)         = AddGuide (f a)
    fmap f (GetCurGuide k)      = GetCurGuide (f . k)
    fmap f (GetGuides k)        = GetGuides (f . k)
    fmap f (SelectJsonFile k)   = SelectJsonFile (f . k)
    fmap f (GetAllRects k)      = GetAllRects (f . k)
    fmap f (SetRects xs a)      = SetRects xs (f a)
    fmap f (OpenJsonFile k)     = OpenJsonFile (f . k)
    fmap f (Active o b a)       = Active o b (f a)
    fmap f (IsActive o k)       = IsActive o (f . k)
    fmap f (SetValuePropVisible b a) = SetValuePropVisible b (f a)
    fmap f (IsToggleActive t k) = IsToggleActive t  (f . k)
    fmap f (SetToggleActive t b a) = SetToggleActive t b (f a)

getSelected :: F DhekInstr (Maybe Rect)
getSelected = wrap $ GetSelected return

setSelected :: Maybe Rect -> F DhekInstr ()
setSelected r = wrap $ SetSelected r (return ())

getRectangles :: F DhekInstr [Rect]
getRectangles = wrap $ GetRectangles return

getCurPage :: F DhekInstr Int
getCurPage = wrap $ GetCurPage return

getPageCount :: F DhekInstr Int
getPageCount = wrap $ GetPageCount return

incrPage :: F DhekInstr ()
incrPage = wrap $ IncrPage (return ())

incrZoom :: F DhekInstr ()
incrZoom = wrap $ IncrZoom (return ())

decrPage :: F DhekInstr ()
decrPage = wrap $ DecrPage (return ())

decrZoom :: F DhekInstr ()
decrZoom = wrap $ DecrZoom (return ())

removeRect :: Rect -> F DhekInstr ()
removeRect r = wrap $ RemoveRect r (return ())

getEntryText :: DhekEntry -> F DhekInstr (Maybe String)
getEntryText e = wrap $ GetEntryText e return

getComboText :: DhekCombo -> F DhekInstr (Maybe String)
getComboText e = wrap $ GetComboText e return

unselectRect :: F DhekInstr ()
unselectRect = wrap $ UnselectRect (return ())

draw :: F DhekInstr ()
draw = wrap $ Draw (return ())

setTitle :: String -> F DhekInstr ()
setTitle t = wrap $ SetTitle t (return ())

getFilename :: F DhekInstr String
getFilename = wrap $ GetFilename return

showError :: String -> F DhekInstr ()
showError e = wrap $ ShowError e (return ())

performIO :: IO b -> F DhekInstr b
performIO o = wrap $ PerformIO o return

getTreeSelection :: F DhekInstr (Maybe Rect)
getTreeSelection = wrap $ GetTreeSelection return

newGuide :: GuideType -> F DhekInstr ()
newGuide g = wrap $ NewGuide g (return ())

updateGuide :: F DhekInstr ()
updateGuide = wrap $ UpdateGuide (return ())

addGuide :: F DhekInstr ()
addGuide = wrap $ AddGuide (return ())

getCurGuide :: F DhekInstr (Maybe Guide)
getCurGuide = wrap $ GetCurGuide return

getGuides :: F DhekInstr [Guide]
getGuides = wrap $ GetGuides return

selectJsonFile :: F DhekInstr (Maybe String)
selectJsonFile = wrap $ SelectJsonFile return

getAllRects :: F DhekInstr [(Int, [Rect])]
getAllRects = wrap $ GetAllRects return

setRects :: [(Int, [Rect])] -> F DhekInstr ()
setRects xs = wrap $ SetRects xs (return ())

openJsonFile :: F DhekInstr (Maybe String)
openJsonFile = wrap $ OpenJsonFile return

active :: DhekOption -> Bool -> F DhekInstr ()
active o b = wrap $ Active o b (return ())

isActive :: DhekOption -> F DhekInstr Bool
isActive o = wrap $ IsActive o return

setValuePropVisible :: Bool -> F DhekInstr ()
setValuePropVisible b = wrap $ SetValuePropVisible b (return ())

isToggleActive :: DhekToggle -> F DhekInstr Bool
isToggleActive t = wrap $ IsToggleActive t return

setToggleActive :: DhekToggle -> Bool -> F DhekInstr ()
setToggleActive t b = wrap $ SetToggleActive t b (return ())
