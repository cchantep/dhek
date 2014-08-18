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
import Control.Concurrent (forkIO)

--------------------------------------------------------------------------------
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
foreign import ccall "util.h nsappTerminate" nsappTerminate :: IO ()
foreign import ccall "util.h nsbrowserOpen" nsbrowserOpen :: CString -> IO ()
foreign import ccall "util.h fcInit" fcInit :: IO CUInt


--------------------------------------------------------------------------------
uiLoaded :: Gtk.Window -> IO Gtk.Window
uiLoaded mainWin = do
  m <- Gtk.messageDialogNew (Just mainWin) [Gtk.DialogModal] 
       Gtk.MessageWarning Gtk.ButtonsNone 
       "Loading system fonts..." -- TODO: i18n
  Gtk.postGUIAsync $ do
    Gtk.dialogRun m
    return ()
  fcInit
  putStrLn "_1"
  Gtk.dialogResponse m Gtk.ResponseNone
  Gtk.widgetHide m
  putStrLn "_2"
  return mainWin

--------------------------------------------------------------------------------
appTerminate :: IO ()
appTerminate = do
  exitSuccess -- ???
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
