{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.AppUtil
--
-- This module declares utilities related to application management,
-- in env which is neither Darwin (Mac OS X) nor Windows (assuming Unix).
--
--------------------------------------------------------------------------------
module Dhek.AppUtil where

--------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
import Dhek.I18N

--------------------------------------------------------------------------------
uiLoaded :: (DhekMessage -> String) -> Gtk.Window -> IO Gtk.Window
uiLoaded _ mainWin = return mainWin

--------------------------------------------------------------------------------
appTerminate :: IO ()
appTerminate = return ()

--------------------------------------------------------------------------------
-- | Given @String@ must be a valid URL
browserOpen :: String -> IO ()
browserOpen _ = return ()

--------------------------------------------------------------------------------
-- | Returns true if given key name is the one of expected modifier
isKeyModifier :: Text -> Bool
isKeyModifier "Control_L" = True
isKeyModifier "Control_R" = True
isKeyModifier _           = False

--------------------------------------------------------------------------------
keyModifierName :: String
keyModifierName = "CTRL"

--------------------------------------------------------------------------------
closeKeystrokes :: Text -> [Gtk.Modifier] -> Bool
closeKeystrokes _ _ = False
