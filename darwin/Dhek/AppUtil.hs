{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.AppUtil
--
-- This module declares application utilities,
-- related to Darwin/Mac OS X integration.
--
--------------------------------------------------------------------------------
module Dhek.AppUtil where

--------------------------------------------------------------------------------
import Foreign.C
import System.Exit (exitSuccess)

--------------------------------------------------------------------------------
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
foreign import ccall "util.h nsappTerminate" nsappTerminate :: IO ()
foreign import ccall "util.h nsbrowserOpen" nsbrowserOpen :: CString -> IO ()

--------------------------------------------------------------------------------
appTerminate :: IO ()
appTerminate = do
  _ <- exitSuccess -- ???
  nsappTerminate

--------------------------------------------------------------------------------
browserOpen :: String -> IO ()
browserOpen url = do
  curl <- newCString url
  nsbrowserOpen curl

--------------------------------------------------------------------------------
-- | Returns true if given key name is the one of expected modifier
isKeyModifier :: String -> Bool
isKeyModifier "Meta_L" = True
isKeyModifier "Meta_R" = True
isKeyModifier _        = False

--------------------------------------------------------------------------------
keyModifierName :: String
keyModifierName = "CMD"

--------------------------------------------------------------------------------
closeKeystrokes :: String -> [Gtk.Modifier] -> Bool
closeKeystrokes "q" [Gtk.Meta] = True
closeKeystrokes _ _            = False
