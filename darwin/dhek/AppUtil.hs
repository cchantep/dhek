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

foreign import ccall "util.h nsappTerminate" appTerminate :: IO ()
