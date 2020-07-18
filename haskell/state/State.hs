{-# LANGUAGE InstanceSigs #-}

--

import Control.Applicative
import Data.Coerce

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap = coerce

instance Applicative Identity where
  pure :: a -> Identity a
  pure = Identity

  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  (<*>) = coerce

instance Monad Identity where
  return :: a -> Identity a
  return = pure

  (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  x >>= f = f (runIdentity x)

--

newtype State s a = State { runState :: s -> (s, a) }

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \s -> (s, ())

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f x = State $ \s -> let (s', a) = runState x s in (s', f a)

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (s, a)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  f <*> x = State $ \s ->
      let (s', f')  = runState f s
          (s'', x') = runState x s'
      in (s'', f' x')

instance Monad (State s) where
  return :: a -> State s a
  return = pure

  (>>=) :: State s a -> (a -> State s b) -> State s b
  x >>= f = State $ \s ->
      let (s', a) = runState x s
      in runState (f a) s'

--

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f m = StateT $ \s ->
   fmap (\ ~(a, s') -> (f a, s')) $ runStateT m s

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  f <*> x = StateT $ \s -> do
    ~(f', s') <- runStateT f s
    ~(a', s'') <- runStateT x s'
    pure (f' a', s'')

instance Monad m => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  x >>= f =  StateT $ \s -> do
    ~(x', s') <- runStateT x s
    runStateT (f x') s'

--

main = undefined
