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
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
appTerminate :: IO ()
appTerminate = return ()

--------------------------------------------------------------------------------
browserOpen :: String -> IO ()
browserOpen url = return ()

--------------------------------------------------------------------------------
-- | Returns true if given key name is the one of expected modifier
isKeyModifier :: String -> Bool
isKeyModifier "Control_L" = True
isKeyModifier "Control_R" = True
isKeyModifier _           = False

--------------------------------------------------------------------------------
keyModifierName :: String
keyModifierName = "CTRL"

--------------------------------------------------------------------------------
closeKeystrokes :: String -> [Gtk.Modifier] -> Bool
closeKeystrokes _ _ = False
