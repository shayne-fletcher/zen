{-# LANGUAGE InstanceSigs #-}

module M where

import Control.Applicative

-- data Maybe' a = Nothing' | Just' a
-- instance Functor Maybe' where
--   fmap :: (a -> b) -> Maybe' a -> Maybe' b
--   fmap f Nothing' = Nothing'
--   fmap f (Just' x) = Just' (f x)
-- instance Applicative Maybe' where
--   pure :: a -> Maybe' a
--   pure = Just'
--   (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
--   Nothing' <*> _ = Nothing'
--   Just' f <*> x = fmap f x
-- liftA2 f x y = fmap f x <*> y

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f = MaybeT . fmap (fmap f) . runMaybeT

instance Applicative m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure = MaybeT . pure . Just

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  f <*> x = MaybeT $ liftA2 (<*>) (runMaybeT f) (runMaybeT x)
  -- Write:
  --   fmap :: (a -> b) -> f2 a -> f2 b
  --   (<*>) :: f1 (a -> b) -> f1 a -> f1 b
  -- Then:
  --   fmap (<*>) :: f2 (f1 (a -> b)) -> f2 (f1 a -> f1 b)
  -- E.g. If f1 = Maybe, f2 = IO, then
  --         fmap (<*>) :: IO (Maybe (a -> b)) -> IO (Maybe a -> Maybe b)
  --      and
  --         fmap (<*>) (runMaybeT f) :: IO (Maybe a -> Maybe b)
  --     so finally,
  --         (fmap (<*>) (runMaybeT f)) <*> (runMaybeT x) :: IO (Maybe b)
  --     that is,
  --         liftA2 (<*>) (runMaybeT f) (runMaybeT x).
  --  Perhaps the intuitive way of looking at this is 'liftA2 (<*>)'
  --  lifts the 'Maybe a' version of '<*>' into 'IO (Maybe a)'.

instance Monad m => Monad (MaybeT m) where
  return :: a -> MaybeT m a
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  x >>= f = MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)

liftIO :: Monad m => m a -> MaybeT m a
liftIO x = MaybeT $ fmap Just x

liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return
