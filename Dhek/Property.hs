module Dhek.Property where

import Prelude hiding (any)

import Control.Applicative ((<*>), (<$>), (<|>))
import Control.Lens ((^.), (.~), (&))
import Control.Monad (when)

import Data.Foldable (any, traverse_)

import Dhek.Instr
import Dhek.Types

onProp :: DhekProgram ()
onProp = compile $ do
    rs <- getRects
    sOpt <- getSelected
    nOpt <- getEntryText PropEntry
    tOpt <- getComboText PropCombo
    traverse_ go ((,,) <$> sOpt <*> nOpt <*> tOpt)
  where
    go (r,n,t) = do
        rs <- getRects
        let r1  = r & rectName .~ n
                    & rectType .~ t
        setSelected (Just r1)
