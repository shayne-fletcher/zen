module Subsets (subsets) where

subsets xs = subsets_ xs (length xs)
  where subsets_ =
         \xs n ->  if (n == 0) then [[]]
               else (subrange_ xs n) ++ (subsets_ xs (n - 1))

subrange_ xs n
  | (n > (length xs)) = []
  | (n == 1) = [[x] | x <- xs]
  | ((length xs) == 0) = [[]]
  | otherwise = 
       [[head xs] ++ y 
          | y <- subrange_ rest (n-1)]  ++ (subrange_ rest n)
  where rest = (tail xs)
