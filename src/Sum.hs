{-# LANGUAGE RankNTypes #-}

module Sum (
  runSum
  ) where

import Data.Functor.Sum (Sum (InL, InR))

runSum
  :: Monad m
  => (forall a. f a -> m a)
  -> (forall a. g a -> m a)
  -> (forall a. (Sum f g) a -> m a)
runSum runF runG = go
  where
    go (InL u) = runF u
    go (InR v) = runG v

