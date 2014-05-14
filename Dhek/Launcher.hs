{-# LANGUAGE ForeignFunctionInterface #-}

module Dhek.Launcher where

import Foreign.C.Types

import Dhek.Engine (makeInterpreter)
import Dhek.GUI (makeGUI, runGUI)
import Dhek.Signal (connectSignals)

launch :: IO ()
launch = do
    gui <- makeGUI
    i   <- makeInterpreter gui
    connectSignals gui i
    runGUI gui

foreign export ccall launch :: IO ()