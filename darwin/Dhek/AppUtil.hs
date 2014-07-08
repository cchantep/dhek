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

import Foreign.C
import System.Exit (exitSuccess)

foreign import ccall "util.h nsappTerminate" nsappTerminate :: IO ()
foreign import ccall "util.h nsbrowserOpen" nsbrowserOpen :: CString -> IO ()

appTerminate :: IO ()
appTerminate = do
  exitSuccess -- ???
  nsappTerminate

browserOpen :: String -> IO ()
browserOpen url = do
  curl <- newCString url
  nsbrowserOpen curl
