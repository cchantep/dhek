{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Engine.Instr
--
-- Engine Intruction
--------------------------------------------------------------------------------
module Dhek.Engine.Instr where

--------------------------------------------------------------------------------
import Control.Applicative

--------------------------------------------------------------------------------
import Control.Monad.Trans

--------------------------------------------------------------------------------
import Dhek.Types

--------------------------------------------------------------------------------
data DhekEntry
    = PropEntry
    | ValueEntry

--------------------------------------------------------------------------------
data DhekCombo = PropCombo

--------------------------------------------------------------------------------
data DhekOption
    = Overlap
    | Magnetic

--------------------------------------------------------------------------------
data Event
    = CreateRect
    | UpdateRectPos
    | DeleteRect
    | UpdateRectProps
    deriving Show

--------------------------------------------------------------------------------
newtype Instr a = Instr (forall m. Runtime m => m a)

--------------------------------------------------------------------------------
class (Applicative m, Monad m, MonadIO m) => Runtime m where
    rGetSelected      :: m (Maybe Rect)
    rGetAllSelected   :: m [Rect]
    rSetSelected      :: Maybe Rect -> m ()
    rGetRectangles    :: m [Rect]
    rGetCurPage       :: m Int
    rGetPageCount     :: m Int
    rIncrPage         :: m ()
    rIncrZoom         :: m ()
    rDecrPage         :: m ()
    rDecrZoom         :: m ()
    rRemoveRect       :: Rect -> m ()
    rUnselectRect     :: m ()
    rDraw             :: m ()
    rSetTitle         :: String -> m ()
    rGetFilename      :: m String
    rShowError        :: String -> m ()
    rGetTreeSelection :: m (Maybe Rect)
    rGuideNew         :: GuideType -> m ()
    rGuideUpdate      :: m ()
    rGuideAdd         :: m ()
    rGuideGetCur      :: m (Maybe Guide)
    rGetGuides        :: m [Guide]
    rSelectJsonFile   :: m (Maybe String)
    rGetAllRects      :: m [(Int, [Rect])]
    rSetAllRects      :: [(Int, [Rect])] -> m ()
    rOpenJsonFile     :: m (Maybe String)
    rActive           :: DhekOption -> Bool -> m ()
    rIsActive         :: DhekOption -> m Bool
    rAddEvent         :: Event -> m ()
    rClearEvents      :: m ()
    rShowWarning      :: String -> m ()

--------------------------------------------------------------------------------
instance Functor Instr where
    fmap f (Instr m) = Instr (fmap f m)

--------------------------------------------------------------------------------
instance Applicative Instr where
    pure a = Instr (pure a)

    Instr f <*> Instr a = Instr (f <*> a)

--------------------------------------------------------------------------------
instance Monad Instr where
    return a = Instr (return a)

    Instr ma >>= f = Instr (ma >>= \a -> let Instr mb = f a in mb)

--------------------------------------------------------------------------------
instance MonadIO Instr where
    liftIO m = Instr $ liftIO m

--------------------------------------------------------------------------------
instance Runtime Instr where
    rGetSelected = getSelected
    rGetAllSelected = getAllSelected
    rSetSelected = setSelected
    rGetRectangles = getRectangles
    rGetCurPage = getCurrentPage
    rGetPageCount = getPageCount
    rIncrPage = incrPage
    rIncrZoom = incrZoom
    rDecrPage = decrPage
    rDecrZoom = decrZoom
    rRemoveRect = removeRect
    rUnselectRect = unselectRect
    rDraw = draw
    rSetTitle = setTitle
    rGetFilename = getFilename
    rShowError = showError
    rGetTreeSelection = getTreeSelection
    rGuideNew = guideNew
    rGuideUpdate = guideUpdate
    rGuideAdd = guideAdd
    rGuideGetCur = guideGetCurrent
    rGetGuides = getGuides
    rSelectJsonFile = selectJsonFile
    rGetAllRects = getAllRects
    rSetAllRects = setAllRects
    rOpenJsonFile = openJsonFile
    rActive = active
    rIsActive = isActive
    rAddEvent = addEvent
    rClearEvents = clearEvents
    rShowWarning = showWarning

--------------------------------------------------------------------------------
getSelected :: Instr (Maybe Rect)
getSelected = Instr rGetSelected

--------------------------------------------------------------------------------
getAllSelected :: Instr [Rect]
getAllSelected = Instr rGetAllSelected

--------------------------------------------------------------------------------
setSelected :: Maybe Rect -> Instr ()
setSelected r = Instr (rSetSelected r)

--------------------------------------------------------------------------------
getRectangles :: Instr [Rect]
getRectangles = Instr rGetRectangles

--------------------------------------------------------------------------------
getCurrentPage :: Instr Int
getCurrentPage = Instr rGetCurPage

--------------------------------------------------------------------------------
getPageCount :: Instr Int
getPageCount = Instr rGetPageCount

--------------------------------------------------------------------------------
incrPage :: Instr ()
incrPage = Instr rIncrPage

--------------------------------------------------------------------------------
incrZoom :: Instr ()
incrZoom = Instr rIncrZoom

--------------------------------------------------------------------------------
decrPage :: Instr ()
decrPage = Instr rDecrPage

--------------------------------------------------------------------------------
decrZoom :: Instr ()
decrZoom = Instr rDecrZoom

--------------------------------------------------------------------------------
removeRect :: Rect -> Instr ()
removeRect r = Instr $ rRemoveRect r

--------------------------------------------------------------------------------
unselectRect :: Instr ()
unselectRect = Instr rUnselectRect

--------------------------------------------------------------------------------
draw :: Instr ()
draw = Instr rDraw

--------------------------------------------------------------------------------
setTitle :: String -> Instr ()
setTitle t = Instr $ rSetTitle t

--------------------------------------------------------------------------------
getFilename :: Instr String
getFilename = Instr rGetFilename

--------------------------------------------------------------------------------
showError :: String -> Instr ()
showError e = Instr $ rShowError e

--------------------------------------------------------------------------------
getTreeSelection :: Instr (Maybe Rect)
getTreeSelection = Instr rGetTreeSelection

--------------------------------------------------------------------------------
guideNew :: GuideType -> Instr ()
guideNew g = Instr $ rGuideNew g

--------------------------------------------------------------------------------
guideUpdate :: Instr ()
guideUpdate = Instr rGuideUpdate

--------------------------------------------------------------------------------
guideAdd :: Instr ()
guideAdd = Instr rGuideAdd

--------------------------------------------------------------------------------
guideGetCurrent :: Instr (Maybe Guide)
guideGetCurrent = Instr rGuideGetCur

--------------------------------------------------------------------------------
getGuides :: Instr [Guide]
getGuides = Instr rGetGuides

--------------------------------------------------------------------------------
selectJsonFile :: Instr (Maybe String)
selectJsonFile = Instr rSelectJsonFile

--------------------------------------------------------------------------------
getAllRects :: Instr [(Int, [Rect])]
getAllRects = Instr rGetAllRects

--------------------------------------------------------------------------------
setAllRects :: [(Int, [Rect])] -> Instr ()
setAllRects xs = Instr $ rSetAllRects xs

--------------------------------------------------------------------------------
openJsonFile :: Instr (Maybe String)
openJsonFile = Instr rOpenJsonFile

--------------------------------------------------------------------------------
active :: DhekOption -> Bool -> Instr ()
active o b = Instr $ rActive o b

--------------------------------------------------------------------------------
isActive :: DhekOption -> Instr Bool
isActive o = Instr $ rIsActive o

--------------------------------------------------------------------------------
addEvent :: Event -> Instr ()
addEvent e = Instr $ rAddEvent e

--------------------------------------------------------------------------------
clearEvents :: Instr ()
clearEvents = Instr rClearEvents

--------------------------------------------------------------------------------
showWarning :: String -> Instr ()
showWarning s = Instr $ rShowWarning s
