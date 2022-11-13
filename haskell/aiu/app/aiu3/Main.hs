{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Main where

-- A natural transformation `f ~> g` is a polymorphic function.
-- η :: ∀ a . Φ a -> Ψ a
type (~>) f g = forall a. f a -> g a
-- The `~>` operator maps a type constructor to another type
-- constructor in such a way that it works for any argument `a` given
-- to the first constructor.
-- e.g. a natural transformation from `Maybe` to `List`.
maybe2List :: Maybe ~> []
maybe2List (Nothing) = []
maybe2List (Just x) = x : []

{-
         Φ(f)
    Φ x -----˃ Φ y
     |          |
  ηx |          | ηy
     |          |
     ˅          ˅
    Ψ x ------˃ Ψ y
         Ψ(f)

(fmap f :: Ψ x -> Ψ y) . (η :: Φ x -> Ψ x)
           ≡ (η :: Φ y -> Ψ y) . (fmap f :: Φ x -> Φ y)
-}

newtype Const a b = Const { unConst :: a }

instance Functor (Const a) where
  fmap :: (b -> c) -> Const a b -> Const a c
  fmap _ (Const a) = Const a

-- newtype Const a b = Const { unConst :: a }
-- instance Functor (Const a) where
--   fmap f c = c

main :: IO ()
main = do
  putStrLn$ "Hello world!"
