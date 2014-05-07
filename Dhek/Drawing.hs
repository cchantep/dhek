{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Drawing
--
-- Draw instruction declaration
--------------------------------------------------------------------------------
module Dhek.Drawing where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad
import Data.Foldable (find)
import Data.Traversable (for)

--------------------------------------------------------------------------------
import           Control.Lens hiding (Zoom)
import           Control.Monad.Reader
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
import Dhek.Engine.Type
import Dhek.Types
