module Types where

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

data Rect = Rect { rectX :: Double
                 , rectY :: Double
                 , rectH :: Double
                 , rectW :: Double } deriving (Eq, Show)

data Field = Field { fieldRect  :: Rect
                   , fieldType  :: String
                   , fieldValue :: String }
