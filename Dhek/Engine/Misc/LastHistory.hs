--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Engine.Misc.LastHistory
--
--------------------------------------------------------------------------------
module Dhek.Engine.Misc.LastHistory
    ( LastHistory
    , lhNew
    , lhPush
    , lhPeek
    , lhPop
    ) where

--------------------------------------------------------------------------------
import Data.Foldable
import Data.List

--------------------------------------------------------------------------------
newtype LastHistory k = LastHistory [k]

--------------------------------------------------------------------------------
instance Foldable LastHistory where
    foldMap f (LastHistory xs) = foldMap f xs

--------------------------------------------------------------------------------
lhNew :: LastHistory k
lhNew = LastHistory []

--------------------------------------------------------------------------------
lhPush :: Eq k => k -> LastHistory k -> LastHistory k
lhPush k (LastHistory ks)
    = LastHistory newKs
  where
    newKs = k : delete k ks

--------------------------------------------------------------------------------
lhPeek :: LastHistory k -> Maybe k
lhPeek = fst . lhPop'

--------------------------------------------------------------------------------
lhPop :: LastHistory k -> LastHistory k
lhPop = snd . lhPop'

--------------------------------------------------------------------------------
lhPop' :: LastHistory k -> (Maybe k, LastHistory k)
lhPop' l@(LastHistory xs)
    = case xs of
          []     -> (Nothing, l)
          x:rest -> (Just x, LastHistory rest)
