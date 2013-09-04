{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Types where

import Control.Arrow (first)
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.Foldable (foldMap)
import Data.IntMap (IntMap, alter)
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
                   , viewerRects          :: IntMap [Rect]
                   , viewerSelectedRect   :: Maybe Rect
                   , viewerThickness      :: Double
                   , viewerSelection      :: Maybe Rect }

data Rect = Rect { rectX      :: Double
                 , rectY      :: Double
                 , rectHeight :: Double
                 , rectWidth  :: Double
                 , rectName   :: String
                 , rectType   :: String } deriving (Eq, Show)

data Save = Save { saveAreas :: [(Int, [Rect])] }

data Field = Field { fieldRect  :: Rect
                   , fieldType  :: String
                   , fieldValue :: String }

instance ToJSON Save where
  toJSON (Save areas) =
    let xsStr = fmap (first (fromString . show)) areas
        toPair (idx, rects) = [idx .= rects]
        pages = object (foldMap toPair xsStr) in
    object ["pages" .= pages]

rectNew :: Double -> Double -> Double -> Double -> Rect
rectNew x y h w = Rect x y h w "noname" "text/checkbox"

addRect :: Int -> Rect -> IntMap [Rect] -> IntMap [Rect]
addRect page x = alter go page
  where
    go (Just xs) = Just (x:xs)
    go _         = Just [x]

$(deriveJSON (fmap toLower . drop 4) ''Rect)
