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
        let msg = "\"" ++ n ++ "\" is already used"
            r1  = r & rectName .~ n
                    & rectType .~ t
            pred o = (o ^. rectName) == n && (o ^. rectId) /= (r ^. rectId)
            exists = any pred rs
        when exists (showError msg)
        when (not exists) (setSelected (Just r1))
