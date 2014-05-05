module Main where

import Dhek.Engine (makeInterpreter)
import Dhek.GUI (makeGUI, runGUI)
import Dhek.Signal (connectSignals)

main :: IO ()
main = do
    gui <- makeGUI
    i   <- makeInterpreter gui
    connectSignals gui i
    runGUI gui
