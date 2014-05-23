module Dhek.Property where

import Prelude hiding (any)

import Control.Applicative ((<*>), (<$>))
import Control.Lens ((.~), (&))

import Data.Foldable (traverse_)

import Dhek.Free
import Dhek.GUI
import Dhek.GUI.Action
import Dhek.Instr
import Dhek.Types

onProp :: GUI -> DhekProgram ()
onProp gui = compile $ do
    sOpt <- getSelected
    nOpt <- getEntryText PropEntry
    vOpt <- getEntryText ValueEntry
    iOpt <- performIO $ gtkGetIndexPropValue gui
    tOpt <- getComboText PropCombo
    traverse_ (go vOpt iOpt) ((,,) <$> sOpt <*> nOpt <*> tOpt)
  where
    go v idx (r,n,t) = do
        let r1  = r & rectName  .~ n
                    & rectType  .~ t
                    & rectValue .~ v
                    & rectIndex .~ idx
        setSelected (Just r1)
        addEvent UpdateRectProps
