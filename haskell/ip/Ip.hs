{-# LANGUAGE ImplicitParams #-}

module Main where

import Data.List (sortBy)

sort :: (?cmp :: a -> a -> Ordering) => [a] -> [a]
sort = sortBy ?cmp

least :: (?cmp::a -> a -> Ordering) => [a] -> a
least xs = head (sort xs)

min_ :: Ord a => [a] -> a
min_ = let ?cmp = compare in least

main = print $ min_ [4, 5, 3]
