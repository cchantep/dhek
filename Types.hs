{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.Aeson.TH
import Data.Char
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
                   , viewerRects          :: [Rect]
                   , viewerSelectedRect   :: Maybe Rect
                   , viewerThickness      :: Double
                   , viewerSelection      :: Maybe Rect }

data Rect = Rect { rectX      :: Double
                 , rectY      :: Double
                 , rectHeight :: Double
                 , rectWidth  :: Double
                 , rectName   :: String
                 , rectType   :: String } deriving (Eq, Show)

data Save = Save { saveResolution :: Double
                 , saveAreas      :: [Rect] }

data Field = Field { fieldRect  :: Rect
                   , fieldType  :: String
                   , fieldValue :: String }

rectNew :: Double -> Double -> Double -> Double -> Rect
rectNew x y h w = Rect x y h w "noname" "text/checkbox"

$(deriveJSON (fmap toLower . drop 4) ''Rect)
$(deriveJSON (fmap toLower . drop 4) ''Save)
