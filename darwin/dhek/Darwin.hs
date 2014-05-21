{-# LANGUAGE ForeignFunctionInterface #-}

--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Darwin
--
-- This module declares utilities related to Darwin/Mac OS X integration.
--
--------------------------------------------------------------------------------
module Dhek.Darwin where

import Foreign.C

foreign import ccall "util.h nsappTerminate" nsappTerminate :: IO ()
