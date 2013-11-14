{-# LANGUAGE ExistentialQuantification #-}
module Dhek.Instr where

import Control.Monad.Free
import Control.Monad.Free.Church

import Data.Foldable (for_, traverse_)

import Dhek.Types

import Graphics.Rendering.Cairo (Render)
import Graphics.UI.Gtk.Gdk.Cursor (CursorType)

type DhekProgram a = Free DhekInstr a

data DhekEntry = PropEntry

data DhekCombo = PropCombo

data DhekOption = Overlap

data DhekInstr a = GetPointer ((Double, Double) -> a)
                 | GetOverRect (Maybe Rect -> a)
                 | GetOverArea (Maybe Area -> a)
                 | GetSelected (Maybe Rect -> a)
                 | SetSelected !(Maybe Rect) a
                 | GetEvent (Maybe BoardEvent -> a)
                 | SetEvent !(Maybe BoardEvent) a
                 | GetRects ([Rect] -> a)
                 | GetRatio (Double -> a)
                 | GetSelection (Maybe Rect -> a)
                 | GetPage (PageItem -> a)
                 | GetCurPage (Int -> a)
                 | GetPageCount (Int -> a)
                 | SetSelection !(Maybe Rect) a
                 | SetCursor !(Maybe CursorType) a
                 | FreshId (Int -> a)
                 | IncrPage a
                 | IncrZoom a
                 | DecrPage a
                 | DecrZoom a
                 | RemoveRect !Rect a
                 | DetachRect !Rect a
                 | AttachRect !Rect a
                 | AddRect !Rect a
                 | GetEntryText !DhekEntry (Maybe String -> a)
                 | GetComboText !DhekCombo (Maybe String -> a)
                 | UnselectRect a
                 | Draw a
                 | SetTitle !String a
                 | GetFilename (String -> a)
                 | GetFrameSize ((Int, Int) -> a)
                 | ExecCairo !(Render ()) a
                 | SizeRequest !Int !Int a
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
                 | PrevPointer ((Double, Double) -> a)
                 | SetCol !(Maybe (Double, Double, Double, Double, Direction)) a
                 | GetCol (Maybe (Double, Double, Double, Double, Direction) -> a)

instance Functor DhekInstr where
    fmap f (GetPointer k)       = GetPointer (f . k)
    fmap f (GetOverRect k)      = GetOverRect (f . k)
    fmap f (GetOverArea k)      = GetOverArea (f . k)
    fmap f (GetSelected k)      = GetSelected (f . k)
    fmap f (SetSelected r a)    = SetSelected r (f a)
    fmap f (GetEvent k)         = GetEvent (f . k)
    fmap f (SetEvent e a)       = SetEvent e (f a)
    fmap f (GetRects k)         = GetRects (f . k)
    fmap f (GetRatio k)         = GetRatio (f . k)
    fmap f (GetSelection k)     = GetSelection (f . k)
    fmap f (GetPage k)          = GetPage (f . k)
    fmap f (GetCurPage k)       = GetCurPage (f . k)
    fmap f (GetPageCount k)     = GetPageCount (f . k)
    fmap f (SetSelection r a)   = SetSelection r (f a)
    fmap f (SetCursor c a)      = SetCursor c (f a)
    fmap f (FreshId k)          = FreshId (f . k)
    fmap f (IncrPage a)         = IncrPage (f a)
    fmap f (DecrPage a)         = DecrPage (f a)
    fmap f (IncrZoom a)         = IncrZoom (f a)
    fmap f (DecrZoom a)         = DecrZoom (f a)
    fmap f (RemoveRect r a)     = RemoveRect r (f a)
    fmap f (DetachRect r a)     = DetachRect r (f a)
    fmap f (AttachRect r a)     = AttachRect r (f a)
    fmap f (AddRect r a)        = AddRect r (f a)
    fmap f (GetEntryText e k)   = GetEntryText e (f . k)
    fmap f (GetComboText e k)   = GetComboText e (f . k)
    fmap f (UnselectRect a)     = UnselectRect (f a)
    fmap f (Draw a)             = Draw (f a)
    fmap f (SetTitle t a)       = SetTitle t (f a)
    fmap f (GetFilename k)      = GetFilename (f . k)
    fmap f (GetFrameSize k)     = GetFrameSize (f . k)
    fmap f (ExecCairo r a)      = ExecCairo r (f a)
    fmap f (SizeRequest x y a)  = SizeRequest x y (f a)
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
    fmap f (PrevPointer k)      = PrevPointer (f . k)
    fmap f (SetCol o a)         = SetCol o (f a)
    fmap f (GetCol k)           = GetCol (f . k)

getPointer :: F DhekInstr (Double, Double)
getPointer = wrap $ GetPointer return

getOverRect :: F DhekInstr (Maybe Rect)
getOverRect = wrap $ GetOverRect return

getOverArea :: F DhekInstr (Maybe Area)
getOverArea = wrap $ GetOverArea return

getSelected :: F DhekInstr (Maybe Rect)
getSelected = wrap $ GetSelected return

setSelected :: Maybe Rect -> F DhekInstr ()
setSelected r = wrap $ SetSelected r (return ())

getEvent :: F DhekInstr (Maybe BoardEvent)
getEvent = wrap $ GetEvent return

setEvent :: Maybe BoardEvent -> F DhekInstr ()
setEvent e = wrap $ SetEvent e (return ())

setEventRect :: Rect -> F DhekInstr ()
setEventRect r = do
    eOpt <- getEvent
    for_ eOpt $ \e ->
        case e of
            Hold _ p     -> setEvent $ Just $ Hold r p
            Resize _ p a -> setEvent $ Just $ Resize r p a

getRects :: F DhekInstr [Rect]
getRects = wrap $ GetRects return

getRatio :: F DhekInstr Double
getRatio = wrap $ GetRatio return

getSelection :: F DhekInstr (Maybe Rect)
getSelection = wrap $ GetSelection return

setSelection :: Maybe Rect -> F DhekInstr ()
setSelection r = wrap $ SetSelection r (return ())

getPage :: F DhekInstr PageItem
getPage = wrap $ GetPage return

getCurPage :: F DhekInstr Int
getCurPage = wrap $ GetCurPage return

getPageCount :: F DhekInstr Int
getPageCount = wrap $ GetPageCount return

setCursor :: Maybe CursorType -> F DhekInstr ()
setCursor c = wrap $ SetCursor c (return ())

freshId :: F DhekInstr Int
freshId = wrap $ FreshId return

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

detachRect :: Rect -> F DhekInstr ()
detachRect r = wrap $ DetachRect r (return ())

attachRect :: Rect -> F DhekInstr ()
attachRect r = wrap $ AttachRect r (return ())

addRect :: Rect -> F DhekInstr ()
addRect r = wrap $ AddRect r (return ())

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

getFrameSize :: F DhekInstr (Int, Int)
getFrameSize = wrap $ GetFrameSize return

execCairo :: Render () -> F DhekInstr ()
execCairo r = wrap $ ExecCairo r (return ())

sizeRequest :: Int -> Int -> F DhekInstr ()
sizeRequest x y = wrap $ SizeRequest x y (return ())

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

prevPointer :: F DhekInstr (Double, Double)
prevPointer = wrap $ PrevPointer return

setCollision :: Maybe (Double, Double, Double, Double, Direction) -> F DhekInstr ()
setCollision o = wrap $ SetCol o (return ())

getCollision :: F DhekInstr (Maybe (Double, Double, Double, Double, Direction))
getCollision = wrap $ GetCol return

compile :: F DhekInstr a -> Free DhekInstr a
compile = fromF

foldFree :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
foldFree f _ (Pure a)  = f a
foldFree g f (Free fa) = f (fmap (foldFree g f) fa)
