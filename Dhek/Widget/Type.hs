{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Widget.Type
--
--
--------------------------------------------------------------------------------
module Dhek.Widget.Type where

--------------------------------------------------------------------------------
type Release = IO ()

--------------------------------------------------------------------------------
data Widget event
    = Widget { widgetRegister :: forall a. event a
                              -> (a -> IO ())
                              -> IO Release

             , widgetShow     :: IO ()
             , widgetHide     :: IO ()
             }
