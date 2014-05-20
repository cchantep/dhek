module Dhek.Property where

import Prelude hiding (any)

import Control.Applicative ((<*>), (<$>), (<|>))
import Control.Lens ((^.), (.~), (&))
import Control.Monad (when)

import Data.Foldable (any, traverse_)

import Dhek.Free
import Dhek.Instr
import Dhek.Types

onProp :: DhekProgram ()
onProp = compile $ do
    sOpt <- getSelected
    nOpt <- getEntryText PropEntry
    vOpt <- getEntryText ValueEntry
    tOpt <- getComboText PropCombo
    traverse_ (go vOpt) ((,,) <$> sOpt <*> nOpt <*> tOpt)
  where
    go v (r,n,t) = do
        rs <- getRectangles
        let r1  = r & rectName  .~ n
                    & rectType  .~ t
                    & rectValue .~ v
        setSelected (Just r1)
        addEvent UpdateRectProps
