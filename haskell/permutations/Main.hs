{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude hiding (concat)

concat :: [[a]] -> [a]
concat = foldr (<>) []

interleave :: a -> [a] -> [[a]]
interleave e [] = [[e]]
interleave e (x : xs) = (e : x : xs) : map (x :) (interleave e xs)

permutations :: [a] -> [[a]]
permutations [] = []
permutations [x] = [[x]]
permutations (x : xs) = concat . map (interleave x) $ permutations xs


main = do
  putStrLn $ show (permutations [1, 2, 3])
