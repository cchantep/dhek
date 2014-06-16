--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Utils
--
--------------------------------------------------------------------------------
module Dhek.Utils where

--------------------------------------------------------------------------------
import Data.Functor.Foldable

--------------------------------------------------------------------------------
-- | Finds a element that satisfy the predicate and deletes it from the list
-- > findDelete (== 2) [1,2,3] == (Just 2, [1,3])
-- > findDelete (== 4) [1,2,3] == (Nothing, [])
-- > findDelete (== 0) []      == (Nothing, [])
findDelete :: (a -> Bool) -> [a] -> (Maybe a, [a])
findDelete p
    = para go where
      go Nil = (Nothing, [])
      go (Cons a (as, res))
          | p a       = (Just a, as)
          | otherwise = let (r, rs) = res in (r, a : rs)
