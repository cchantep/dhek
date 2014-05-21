{-# LANGUAGE ForeignFunctionInterface #-}

--------------------------------------------------------------------------------
-- |
-- Module : Dhek.AppUtil
--
-- This module declares utilities related to Windows integration.
--
--------------------------------------------------------------------------------
module Dhek.AppUtil where

import Foreign.C
import Foreign.C.String

foreign import ccall "util.h browser_open" browser_open :: CString -> IO ()

appTerminate :: IO ()
appTerminate = return ()

browserOpen :: String -> IO ()
browserOpen url = do
  curl <- newCString url
  browser_open curl
  

