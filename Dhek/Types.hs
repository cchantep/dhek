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
import           Data.Monoid (Monoid (..), (<>))
import           Data.Text (Text, pack)
import qualified Data.Vector                      as V
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler

--------------------------------------------------------------------------------
import Dhek.Cartesian
import Dhek.Version

--------------------------------------------------------------------------------
-- | Declarations
--------------------------------------------------------------------------------
data Viewer
    = Viewer
      { _viewerDocument  :: Poppler.Document
      , _viewerPages     :: Array Int PageItem
      , _viewerPageCount :: Int
      , _viewerName      :: String
      }

--------------------------------------------------------------------------------
data Board
    = Board
      { _boardRects  :: !(IntMap Rect)
      , _boardGuides :: ![Guide]
      }
    deriving Show

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
data Boards
    = Boards
      { _boardsState :: !Int
      , _boardsMap   :: !(IntMap Board)
      }

--------------------------------------------------------------------------------
data Grouped
    = Simple Rect
    | Grouped Text GType [Rect]

--------------------------------------------------------------------------------
data GType = TextCell

--------------------------------------------------------------------------------
data Rect
    = Rect
      { _rectId     :: !Int
      , _rectPoint  :: !Point2D
      , _rectHeight :: !Double
      , _rectWidth  :: !Double
      , _rectName   :: !Text
      , _rectType   :: !Text
      , _rectValue  :: !(Maybe Text)
      , _rectIndex  :: !(Maybe Int)
      }
    deriving (Eq, Show)

--------------------------------------------------------------------------------
data GuideType
    = GuideVertical
    | GuideHorizontal
    deriving Show

--------------------------------------------------------------------------------
data Guide
    = Guide
      { _guideValue :: !Double
      , _guideType  :: !GuideType
      }
    deriving Show

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
rectX :: Lens Rect Rect Double Double
rectX = lens (^. rectPoint.pointX) $ \r d -> r & rectPoint.pointX .~ d

--------------------------------------------------------------------------------
rectY :: Lens Rect Rect Double Double
rectY = lens (^. rectPoint.pointY) $ \r d -> r & rectPoint.pointY .~ d

--------------------------------------------------------------------------------
-- | Instances
--------------------------------------------------------------------------------
instance Monoid Board where
    mempty = Board empty []
    mappend (Board l g) (Board r g') = Board (mappend l r) (g ++ g')

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
        pt    = r ^. rectPoint
        vProp = "value" .= _rectValue r
        iProp = "index" .= _rectIndex r

        props = [ "x"      .= (pt ^. pointX)
                , "y"      .= (pt ^. pointY)
                , "height" .= (r ^. rectHeight)
                , "width"  .= (r ^. rectWidth)
                , "name"   .= (r ^. rectName)
                , "type"   .= (r ^. rectType)
                ]

--------------------------------------------------------------------------------
instance FromJSON Rect where
    parseJSON (Object v) =
        Rect 0            <$>
        fromJsonPoint2B v <*>
        v .:  "height"    <*>
        v .:  "width"     <*>
        v .:  "name"      <*>
        v .:  "type"      <*>
        v .:? "value"     <*>
        v .:? "index"
    parseJSON _ = mzero

--------------------------------------------------------------------------------
fromJsonPoint2B :: Object -> Parser Point2D
fromJsonPoint2B v
    = point2D <$> v .: "x" <*> v .: "y"

--------------------------------------------------------------------------------
toJsonGrouped :: Grouped -> Value
toJsonGrouped (Simple r)
    = toJSON r
toJsonGrouped (Grouped name typ rs)
    = case typ of
    TextCell -> toJsonTextCell name rs

--------------------------------------------------------------------------------
toJsonTextCell :: Text -> [Rect] -> Value
toJsonTextCell name rs
    = object [ "type"  .= ("celltext" :: String)
             , "name"  .= name
             , "cells" .= fmap toCell rs
             ]
  where
    toCell r
        = object [ "x"      .= (r ^. rectX)
                 , "y"      .= (r ^. rectY)
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
                                 , _rectPoint  = point2D x y
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
boardsNew n = Boards 0 maps
  where
    maps = fromList $ fmap (\i -> (i, Board empty [])) [1..n]

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
rectNew :: Point2D -> Double -> Double -> Rect
rectNew pt h w
    = Rect
      { _rectId     = 0
      , _rectPoint  = pt
      , _rectHeight = h
      , _rectWidth  = w
      , _rectName   = "field"
      , _rectType   = "text"
      , _rectValue  = Nothing
      , _rectIndex  = Nothing
      }

--------------------------------------------------------------------------------
normalize :: Rect -> Rect
normalize r = newRectY newRectX
  where
    w = r ^. rectWidth
    h = r ^. rectHeight

    newRectX
        | w < 0 = r & rectX     +~ w
                    & rectWidth .~ abs w
        | otherwise = r

    newRectY xr
        | h < 0 = xr & rectY      +~ h
                     & rectHeight .~ abs h
        | otherwise = xr

--------------------------------------------------------------------------------
addRect :: Int -> Rect -> Boards -> Boards
addRect page x = execState action
  where
    action = do
        i <- use boardsState
        boardsState += 1
        let x' = x & rectId .~ i & rectName %~ (<> (pack $ show i))
        (boardsMap.at page.traverse.boardRects.at i) ?= x'

--------------------------------------------------------------------------------
rectArea :: Double -> Rect -> Area -> Rect
rectArea eps r area = go area
  where
    x = r ^. rectX
    y = r ^. rectY
    h = r ^. rectHeight
    w = r ^. rectWidth

    go TOP_LEFT     = rectNew (point2D x y) eps eps
    go TOP          = rectNew (point2D (x+eps) y) eps (w-2*eps)
    go TOP_RIGHT    = rectNew (point2D (x+w-eps) y) eps eps
    go RIGHT        = rectNew (point2D (x+w-eps) (y+eps)) (h-2*eps) eps
    go BOTTOM_RIGHT = rectNew (point2D (x+w-eps) (y+h-eps)) eps eps
    go BOTTOM       = rectNew (point2D (x+eps) (y+h-eps)) eps (w-2*eps)
    go BOTTOM_LEFT  = rectNew (point2D x (y+h-eps)) eps eps
    go LEFT         = rectNew (point2D x (y+eps)) (h-2*eps) eps


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
