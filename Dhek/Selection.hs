module Dhek.Selection where

import Dhek.Free
import Dhek.Instr

onSel :: DhekProgram ()
onSel = compile $ do
    rOpt <- getTreeSelection
    setSelected rOpt
    draw
