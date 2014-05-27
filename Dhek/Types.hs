{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Types
--
-- Main type declaration and intances
--------------------------------------------------------------------------------
module Dhek.Types where

--------------------------------------------------------------------------------
import Control.Applicative ((<*>), (<$>))
import Control.Monad (mzero)
import Control.Monad.State (execState, evalStateT, modify, get)
import Control.Monad.Trans (lift)
import Data.Array (Array)
import Data.Foldable (foldMap)
import Data.List (sortBy)

--------------------------------------------------------------------------------
import           Control.Lens hiding ((.=))
import           Data.Aeson hiding (Array)
import           Data.Aeson.Types (Parser)
import           Data.IntMap (IntMap, empty, fromList)
import qualified Data.Map                         as Map
import           Data.Monoid (Monoid (..))
import qualified Data.Vector                      as V
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler

--------------------------------------------------------------------------------
import Dhek.Version

--------------------------------------------------------------------------------
-- | Declarations
--------------------------------------------------------------------------------
data Viewer
    = Viewer
      { _viewerDocument  :: Poppler.Document
      , _viewerPages     :: Array Int PageItem
      , _viewerPageCount :: Int
      }

--------------------------------------------------------------------------------
data Board
    = Board
      { _boardRects :: !(IntMap Rect) }
    deriving Show

--------------------------------------------------------------------------------
data BoardEvent
    = None
    | Hold Rect (Double, Double)
    | Resize Rect (Double, Double) Area
    deriving (Show, Eq)

--------------------------------------------------------------------------------
data Area
    = TOP_LEFT
    | TOP
    | TOP_RIGHT
    | RIGHT
    | BOTTOM_RIGHT
    | BOTTOM
    | BOTTOM_LEFT
    | LEFT
    deriving (Enum, Show, Eq)

--------------------------------------------------------------------------------
data Direction
    = NORTH
    | EAST
    | SOUTH
    | WEST
    deriving (Eq, Show)

--------------------------------------------------------------------------------
data Boards
    = Boards
      { _boardsState     :: !Int
      , _boardsEvent     :: !BoardEvent
      , _boardsSelection :: !(Maybe Rect)
      , _boardsOvered    :: !(Maybe Rect)
      , _boardsSelected  :: !(Maybe Rect)
      , _boardsCurGuide  :: !(Maybe Guide)
      , _boardsGuides    :: ![Guide]
      , _boardsMap       :: !(IntMap Board)
      }

--------------------------------------------------------------------------------
data Grouped
    = Simple Rect
    | Grouped String GType [Rect]

--------------------------------------------------------------------------------
data GType = TextCell

--------------------------------------------------------------------------------
data Rect
    = Rect
      { _rectId     :: !Int
      , _rectX      :: !Double
      , _rectY      :: !Double
      , _rectHeight :: !Double
      , _rectWidth  :: !Double
      , _rectName   :: !String
      , _rectType   :: !String
      , _rectValue  :: !(Maybe String)
      , _rectIndex  :: !(Maybe Int)
      }
    deriving (Eq, Show)

--------------------------------------------------------------------------------
data GuideType
    = GuideVertical
    | GuideHorizontal

--------------------------------------------------------------------------------
data Guide
    = Guide
      { _guideValue :: !Double
      , _guideType  :: !GuideType
      }

--------------------------------------------------------------------------------
data Save
    = Save
      { saveVersion :: !String
      , saveAreas   :: ![(Int, [Grouped])]
      }

--------------------------------------------------------------------------------
data PageItem
    = PageItem
      { pagePtr    :: !Poppler.Page
      , pageWidth  :: !Double
      , pageHeight :: !Double
      }

--------------------------------------------------------------------------------
-- | Lens declarations
--------------------------------------------------------------------------------
makeLenses ''Viewer
makeLenses ''Board
makeLenses ''Boards
makeLenses ''Rect
makeLenses ''Guide

--------------------------------------------------------------------------------
-- | Instances
--------------------------------------------------------------------------------
instance Monoid Board where
    mempty = Board empty
    mappend (Board l) (Board r) = Board (mappend l r)

--------------------------------------------------------------------------------
instance ToJSON Save where
    toJSON (Save v areas) =
        let toPage (_, rects)
                | null rects = Null
                | otherwise  = object ["areas" .= fmap toJsonGrouped rects]
            pages = fmap toPage areas in
        object ["format" .= v, "pages" .= pages]

--------------------------------------------------------------------------------
instance FromJSON Save where
    parseJSON (Object v) = do
        ver   <- v .: "format"
        pages <- v .: "pages"
        areas <- withArray "list of pages" (go . V.toList) pages
        return (Save ver areas)
      where
        go xs = evalStateT (traverse pageToAreas xs) 0

        pageToAreas opt = do
            modify (+1)
            i <- get
            case opt of
              Null -> return (i, [])
              _    -> do
                  xs <- lift $ withObject "page" extractAreas opt
                  return (i, xs)

        extractAreas obj = do
            areas <- obj .: "areas"
            withArray "areas" (toAreas . V.toList) areas

        toAreas = traverse parseGrouped
    parseJSON _ = mzero

--------------------------------------------------------------------------------
instance ToJSON Rect where
    toJSON r =
        object $
        let ps  = maybe props (const $ vProp:props) (_rectValue r)
            ps' = maybe ps (const $ iProp:ps) (_rectIndex r) in
        ps'
      where
        vProp = "value" .= _rectValue r
        iProp = "index" .= _rectIndex r

        props = [ "x"      .= _rectX r
                , "y"      .= _rectY r
                , "height" .= _rectHeight r
                , "width"  .= _rectWidth r
                , "name"   .= _rectName r
                , "type"   .= _rectType r
                ]

--------------------------------------------------------------------------------
instance FromJSON Rect where
    parseJSON (Object v) =
        Rect 0         <$>
        v .:  "x"      <*>
        v .:  "y"      <*>
        v .:  "height" <*>
        v .:  "width"  <*>
        v .:  "name"   <*>
        v .:  "type"   <*>
        v .:? "value"  <*>
        v .:? "index"
    parseJSON _ = mzero

--------------------------------------------------------------------------------
toJsonGrouped :: Grouped -> Value
toJsonGrouped (Simple r)
    = toJSON r
toJsonGrouped (Grouped name typ rs)
    = case typ of
    TextCell -> toJsonTextCell name rs

--------------------------------------------------------------------------------
toJsonTextCell :: String -> [Rect] -> Value
toJsonTextCell name rs
    = object [ "type"  .= ("celltext" :: String)
             , "name"  .= name
             , "cells" .= fmap toCell rs
             ]
  where
    toCell r
        = object [ "x"      .= _rectX r
                 , "y"      .= _rectY r
                 , "width"  .= _rectWidth r
                 , "height" .= _rectHeight r
                 , "index"  .= _rectIndex r
                 ]

--------------------------------------------------------------------------------
parseGrouped :: Value -> Parser Grouped
parseGrouped obj@(Object v)
    = do typ <- v .: "type"
         case (typ :: String) of
             "celltext" -> parseTextCell obj
             _          -> parseSimple obj
parseGrouped _
    = mzero

--------------------------------------------------------------------------------
parseSimple :: Value -> Parser Grouped
parseSimple = fmap Simple . parseJSON

--------------------------------------------------------------------------------
parseTextCell :: Value -> Parser Grouped
parseTextCell (Object v)
    = do name  <- v .: "name"
         cells <- v .: "cells"
         let fromCell (Object c)
                 = do x <- c .: "x"
                      y <- c .: "y"
                      w <- c .: "width"
                      h <- c .: "height"
                      i <- c .: "index"
                      return Rect{ _rectId     = 0
                                 , _rectX      = x
                                 , _rectY      = y
                                 , _rectType   = "textcell"
                                 , _rectWidth  = w
                                 , _rectHeight = h
                                 , _rectIndex  = Just i
                                 , _rectValue  = Nothing
                                 , _rectName   = name
                                 }
             fromCell _ = mzero
         rs <- traverse fromCell (cells :: [Value])
         return $ Grouped name TextCell rs
parseTextCell _
    = mzero

--------------------------------------------------------------------------------
saveNew :: [(Int, [Rect])] -> Save
saveNew xs = Save dhekFullVersion xs' where
  xs' = fmap (\(i,rs) -> (i, classifyRects rs)) xs

--------------------------------------------------------------------------------
boardsNew :: Int -> Boards
boardsNew n = Boards 0 None Nothing Nothing Nothing Nothing [] maps
  where
    maps = fromList $ fmap (\i -> (i, Board empty)) [1..n]

--------------------------------------------------------------------------------
fillUp :: Int -> [(Int, [Rect])] -> [(Int, [Rect])]
fillUp n xs = go xs [1..n]
  where
    go [] is = fmap (\i -> (i, [])) is
    go _ []  = []
    go v@((k, rs):rest) (i:is)
        | k == i = (k, rs) : go rest is
        | k > i  = (i, []) : go v is
    go _ _ = error "impossible situation in Types.fillUp"

--------------------------------------------------------------------------------
rectNew :: Double -> Double -> Double -> Double -> Rect
rectNew x y h w = Rect 0 x y h w "field" "text" Nothing Nothing

--------------------------------------------------------------------------------
translateRect :: Double -> Double -> Rect -> Rect
translateRect x y r = r & rectX +~ x & rectY +~ y

--------------------------------------------------------------------------------
translateRectX :: Double -> Rect -> Rect
translateRectX x r = r & rectX +~ x

--------------------------------------------------------------------------------
translateRectY :: Double -> Rect -> Rect
translateRectY y r = r & rectY +~ y

--------------------------------------------------------------------------------
eventGetRect :: BoardEvent -> Maybe Rect
eventGetRect (Hold r _)     = Just r
eventGetRect (Resize r _ _) = Just r
eventGetRect _              = Nothing

--------------------------------------------------------------------------------
eventSetRect :: Rect -> BoardEvent -> BoardEvent
eventSetRect r (Hold _ x)     = Hold r x
eventSetRect r (Resize _ x y) = Resize r x y
eventSetRect _ e              = e

--------------------------------------------------------------------------------
normalize :: Rect -> Rect
normalize r = newRectY newRectX
  where
    x = _rectX r
    y = _rectY r
    w = _rectWidth r
    h = _rectHeight r

    newRectX
        | w < 0     = r { _rectX = x + w, _rectWidth = abs w }
        | otherwise = r

    newRectY r
        | h < 0     = r { _rectY = y + h, _rectHeight = abs h }
        | otherwise = r

--------------------------------------------------------------------------------
addRect :: Int -> Rect -> Boards -> Boards
addRect page x = execState action
  where
    action = do
        i <- use boardsState
        boardsState += 1
        let x' = x & rectId .~ i & rectName %~ (++ show i)
        (boardsMap.at page.traverse.boardRects.at i) ?= x'

--------------------------------------------------------------------------------
rectArea :: Double -> Rect -> Area -> Rect
rectArea eps r area = go area
  where
    x = r ^. rectX
    y = r ^. rectY
    h = r ^. rectHeight
    w = r ^. rectWidth

    go TOP_LEFT     = rectNew x y eps eps
    go TOP          = rectNew (x+eps) y eps (w-2*eps)
    go TOP_RIGHT    = rectNew (x+w-eps) y eps eps
    go RIGHT        = rectNew (x+w-eps) (y+eps) (h-2*eps) eps
    go BOTTOM_RIGHT = rectNew (x+w-eps) (y+h-eps) eps eps
    go BOTTOM       = rectNew (x+eps) (y+h-eps) eps (w-2*eps)
    go BOTTOM_LEFT  = rectNew x (y+h-eps) eps eps
    go LEFT         = rectNew x (y+eps) (h-2*eps) eps

--------------------------------------------------------------------------------
-- | Using Minkowski Sum
rectIntersect :: Rect -> Rect -> Maybe Direction
rectIntersect a b
    | not collides = Nothing
    | otherwise =
        if wy > hx
        then if wy > negate hx
             then Just SOUTH
             else Just WEST
        else if wy > negate hx
             then Just EAST
             else Just NORTH
  where
    aw = a ^. rectWidth
    ah = a ^. rectHeight
    ax = a ^. rectX
    ay = a ^. rectY
    acenterx = ax + aw / 2
    acentery = ay + ah / 2

    bw = b ^. rectWidth
    bh = b ^. rectHeight
    bx = b ^. rectX
    by = b ^. rectY
    bcenterx = bx + bw / 2
    bcentery = by + bh / 2

    w  = 0.5 * (aw + bw)
    h  = 0.5 * (ah + bh)
    dx = acenterx - bcenterx
    dy = acentery - bcentery

    collides = abs dx <= w && abs dy <= h

    wy = w * dy
    hx = h * dx

--------------------------------------------------------------------------------
modifyEventRect :: (Rect -> Rect) -> BoardEvent -> BoardEvent
modifyEventRect k (Hold r p)     = Hold (k r) p
modifyEventRect k (Resize r p a) = Resize (k r) p a

--------------------------------------------------------------------------------
oppositeDirection :: Direction -> Direction
oppositeDirection NORTH = SOUTH
oppositeDirection SOUTH = NORTH
oppositeDirection EAST  = WEST
oppositeDirection WEST  = EAST

--------------------------------------------------------------------------------
sameRectId :: Rect -> Rect -> Bool
sameRectId r x = (x ^. rectId) == (r ^. rectId)

--------------------------------------------------------------------------------
classifyRects :: [Rect] -> [Grouped]
classifyRects = loop Map.empty where
  loop m []
      = let toGroup (name, xs)
                = Grouped name TextCell $ sortBy sortF xs
            sortF l r
                = compare (l ^. rectIndex) (r ^. rectIndex) in
        fmap toGroup (Map.toList m)
  loop m (r:rs)
      | r ^. rectType /= "textcell" = Simple r : loop m rs
      | otherwise =
          let key = r ^. rectName
              m'  = Map.insertWith (++) key [r] m in
          loop m' rs

--------------------------------------------------------------------------------
expandGrouped :: [Grouped] -> [Rect]
expandGrouped = foldMap go where
  go (Simple r)       = [r]
  go (Grouped _ _ rs) = rs

--------------------------------------------------------------------------------
rectCompareX :: Rect -> Rect -> Ordering
rectCompareX a b
    = compare (a ^. rectX) (b ^. rectX)

--------------------------------------------------------------------------------
rectCompareIndex :: Rect -> Rect -> Ordering
rectCompareIndex a b
    = compare (a ^. rectIndex) (b ^. rectIndex)
