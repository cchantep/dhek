{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Types where

import Control.Arrow (first)
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.Foldable (foldMap)
import Data.IntMap (IntMap, alter, empty)
import Data.String (fromString)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Poppler.Document (Document)

data Viewer =
            Viewer { viewerArea           :: DrawingArea
                   , viewerDocument       :: Document
                   , viewerScrolledWindow :: ScrolledWindow
                   , viewerCurrentPage    :: Int
                   , viewerPageCount      :: Int
                   , viewerZoom           :: Double
                   , viewerBaseWidth      :: Int
                   , viewerStore          :: RectStore
                   , viewerSelectedRect   :: Maybe Rect
                   , viewerThickness      :: Double
                   , viewerSelection      :: Maybe Rect }

data RectStore = RectStore { rstoreState :: Int
                           , rstoreRects :: IntMap [Rect] }

data Rect = Rect { rectX      :: Double
                 , rectY      :: Double
                 , rectHeight :: Double
                 , rectWidth  :: Double
                 , rectName   :: String
                 , rectType   :: String } deriving (Eq, Show)

data Save = Save { saveAreas :: [(Int, Maybe [Rect])] }

data Field = Field { fieldRect  :: Rect
                   , fieldType  :: String
                   , fieldValue :: String }

instance ToJSON Save where
  toJSON (Save areas) =
    let toPage (_, rects) = maybe Null (\t -> object ["areas" .= t]) rects
        pages = fmap toPage areas in
    object ["pages" .= pages]

emptyStore :: RectStore
emptyStore = RectStore 1 empty

fillUp :: Int -> [(Int, [Rect])] -> [(Int, Maybe [Rect])]
fillUp n xs = go xs [0..(n - 1)]
  where
    go [] is = fmap (\i -> (i, Nothing)) is
    go v@((k, rs):xs) (i:is)
       | k == i = (k, Just rs) : go xs is
       | k > i  = (i, Nothing) : go v is

rectNew :: Double -> Double -> Double -> Double -> Rect
rectNew x y h w = Rect x y h w "field" "text/checkbox"

addRect :: Int -> Rect -> RectStore -> RectStore
addRect page x (RectStore i rects) = RectStore (i+1) newRects
  where
    newRects = alter go page rects
    x'       = x { rectName = rectName x ++ (show i) }

    go (Just xs) = Just (x':xs)
    go _         = Just [x']

$(deriveJSON (fmap toLower . drop 4) ''Rect)
