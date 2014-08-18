{-# LANGUAGE ForeignFunctionInterface #-}

module Dhek.Launcher where

import Dhek.Engine (makeRuntimeEnv)
import Dhek.GUI (makeGUI, runGUI)
import Dhek.Signal (connectSignals)

launch :: IO ()
launch = do
    gui <- makeGUI
    i   <- makeRuntimeEnv gui
    connectSignals gui i
    runGUI gui

foreign export ccall launch :: IO ()
