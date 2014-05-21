{-# INCLUDE "../darwin/util.h" #-}
{-# LANGUAGE ForeignFunctionInterface #-}

--------------------------------------------------------------------------------
-- |
-- Module : Dhek.DarwinUtil
--
-- This module declares everything related to the GUI like widgets
--
--------------------------------------------------------------------------------
module Dhek.DarwinUtil where

import Foreign.C

foreign import ccall "nsappTerminate" nsappTerminate :: IO ()
