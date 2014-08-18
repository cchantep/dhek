{-# LANGUAGE ForeignFunctionInterface #-}

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
import Foreign.C.String

import qualified Graphics.UI.Gtk as Gtk

foreign import ccall "util.h browser_open" browser_open :: CString -> IO ()

--------------------------------------------------------------------------------
uiLoaded :: Gtk.Window -> IO Gtk.Window
uiLoaded mainWin = return mainWin

--------------------------------------------------------------------------------
appTerminate :: IO ()
appTerminate = return ()

--------------------------------------------------------------------------------
browserOpen :: String -> IO ()
browserOpen url = do
  curl <- newCString url
  browser_open curl

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
closeKeystrokes "F4" [Gtk.Alt] = True
closeKeystrokes _ _            = False
