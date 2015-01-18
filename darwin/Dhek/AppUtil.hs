{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
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
import Data.IORef
import Foreign.C
import System.Exit (exitSuccess)

--------------------------------------------------------------------------------
import           Data.Text
import qualified Graphics.UI.Gtk as Gtk

--------------------------------------------------------------------------------
import Dhek.I18N

--------------------------------------------------------------------------------
foreign import ccall "util.h nsappTerminate" nsappTerminate :: IO ()
foreign import ccall "util.h nsbrowserOpen" nsbrowserOpen :: CString -> IO ()
foreign import ccall "util.h fcInit" fcInit :: IO CUInt

--------------------------------------------------------------------------------
uiLoaded :: (DhekMessage -> String) -> Gtk.Window -> IO Gtk.Window
uiLoaded mkStr mainWin = do
  m <- splashScreen mkStr mainWin

  ref <- newIORef undefined
  c   <- Gtk.after mainWin Gtk.mapSignal $
           do Gtk.widgetShow m

              Gtk.postGUIAsync $
                  do _ <- fcInit
                     Gtk.dialogResponse m Gtk.ResponseNone
                     cid <- readIORef ref
                     Gtk.signalDisconnect cid

  writeIORef ref c
  return mainWin

--------------------------------------------------------------------------------
splashScreen :: (DhekMessage -> String) -> Gtk.Window -> IO Gtk.MessageDialog
splashScreen mkStr win
    = do m <- Gtk.messageDialogNew (Just win) [Gtk.DialogModal]
              Gtk.MessageWarning Gtk.ButtonsNone
              (mkStr MsgFontCacheInit)

         _ <- Gtk.on m Gtk.response $ \_ ->
                  Gtk.widgetDestroy m

         return m

--------------------------------------------------------------------------------
appTerminate :: IO ()
appTerminate = do
  _ <- exitSuccess -- ???
  nsappTerminate

--------------------------------------------------------------------------------
browserOpen :: String -> IO ()
browserOpen url = withCString url nsbrowserOpen

--------------------------------------------------------------------------------
-- | Returns true if given key name is the one of expected modifier
isKeyModifier :: Text -> Bool
isKeyModifier "Meta_L" = True
isKeyModifier "Meta_R" = True
isKeyModifier _        = False

--------------------------------------------------------------------------------
keyModifierName :: String
keyModifierName = "CMD"

--------------------------------------------------------------------------------
closeKeystrokes :: Text -> [Gtk.Modifier] -> Bool
closeKeystrokes "q" [Gtk.Meta] = True
closeKeystrokes _ _            = False
