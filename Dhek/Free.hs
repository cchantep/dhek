--------------------------------------------------------------------------------
-- |
-- Module : Dhek.Free
--
-- Free monad utilities
--
--------------------------------------------------------------------------------
module Dhek.Free where

--------------------------------------------------------------------------------
import Control.Monad.Free
import Control.Monad.Free.Church

--------------------------------------------------------------------------------
compile :: Functor f => F f a -> Free f a
compile = fromF

--------------------------------------------------------------------------------
-- | Folds a Free structure from the right to left
foldFree :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
foldFree f _ (Pure a)  = f a
foldFree f g (Free fa) = g $ fmap (foldFree f g) fa
