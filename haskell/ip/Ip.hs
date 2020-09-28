{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.List (sortBy)
import Prelude hiding (min)

sort :: (?cmp :: a -> a -> Ordering) => [a] -> [a]
sort = sortBy ?cmp

least :: (?cmp::a -> a -> Ordering) => [a] -> a
least xs = head (sort xs)

min :: Ord a => [a] -> a
min = let ?cmp = compare in least

main = print $ min [4, 5, 3]
