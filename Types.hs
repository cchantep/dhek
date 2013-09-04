{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.Aeson.TH
import Data.Char
import Data.IntMap (IntMap, alter)
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
                 , rectType   :: String
                 , rectPage   :: Int } deriving (Eq, Show)

data Save = Save { saveResolution :: Double
                 , saveAreas      :: [Rect] }

data Field = Field { fieldRect  :: Rect
                   , fieldType  :: String
                   , fieldValue :: String }

rectNew :: Double -> Double -> Double -> Double -> Int -> Rect
rectNew x y h w page = Rect x y h w "noname" "text/checkbox" page

addRect :: Rect -> IntMap [Rect] -> IntMap [Rect]
addRect x = alter go page
  where
    go (Just xs) = Just (x:xs)
    go _         = Just [x]

    page = rectPage x

$(deriveJSON (fmap toLower . drop 4) ''Rect)
$(deriveJSON (fmap toLower . drop 4) ''Save)
