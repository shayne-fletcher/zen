{-# LANGUAGE InstanceSigs #-}

module Reader where

import Control.Applicative

newtype Reader s a = Reader { runReader :: s -> a }

instance Functor (Reader s) where
  fmap :: (a -> b) -> Reader s a -> Reader s b
  fmap f = Reader . fmap f . runReader

instance Applicative (Reader s) where
  pure :: a -> Reader s a
  pure = Reader . const

  (<*>) :: Reader s (a -> b) -> Reader s a -> Reader s b
  f <*> x = Reader $ \ s ->
    let f' = runReader f s
        x' = runReader x s
    in f' x'

instance Monad (Reader s) where
  return :: a -> Reader s a
  return = pure

  (>>=) :: Reader s a -> (a -> Reader s b) -> Reader s b
  x >>= f = Reader $ \ s -> runReader (f (runReader x s)) s

ask :: Reader s s
ask = Reader $ \ s -> s
