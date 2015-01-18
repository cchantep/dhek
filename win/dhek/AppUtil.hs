{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.AppUtil
--
-- This module declares utilities related to Windows integration.
--
--------------------------------------------------------------------------------
module Dhek.AppUtil where

--------------------------------------------------------------------------------
import Foreign.C

--------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
import Dhek.I18N

--------------------------------------------------------------------------------
foreign import ccall "util.h browser_open" browser_open :: CString -> IO ()

--------------------------------------------------------------------------------
uiLoaded :: (DhekMessage -> String) -> Gtk.Window -> IO Gtk.Window
uiLoaded _ mainWin = return mainWin

--------------------------------------------------------------------------------
appTerminate :: IO ()
appTerminate = return ()

--------------------------------------------------------------------------------
browserOpen :: String -> IO ()
browserOpen url = withCString url browser_open

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
closeKeystrokes "F4" [Gtk.Alt] = True
closeKeystrokes _ _            = False
