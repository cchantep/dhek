--------------------------------------------------------------------------------
-- |
-- Module : Dhek.AppUtil
--
-- This module declares utilities related to application management,
-- in env which is neither Darwin (Mac OS X) nor Windows (assuming Unix).
--
--------------------------------------------------------------------------------
module Dhek.AppUtil where

appTerminate :: IO ()
appTerminate = return ()

browserOpen :: String -> IO ()
browserOpen url = return ()
