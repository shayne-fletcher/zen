module Interpolation (
    lower_bound
  , upper_bound
  , equal_range
  , bound
  , linear_interpolation
  , loglinear_interpolation)
where

-- | lower_bound
-- | 
-- | Finds the first position in xs where x could be inserted
-- | without violating the ordering.

lower_bound :: (Ord a) => a -> [a] -> Int
lower_bound_ x xs first count
  | count == 0      = first
  | (xs !! mid < x) = lower_bound_ x xs (mid+1) (count-half-1)
  | otherwise       = lower_bound_ x xs first half 
  where half = count `div` 2;  mid = first + half

lower_bound x xs  = lower_bound_ x xs 0 (length xs)

-- | upper_bound
-- | 
-- | Finds the last position in xs where x could be inserted
-- | without violating the ordering.

upper_bound :: (Ord a) => a -> [a] -> Int
upper_bound_ x xs first count
   | count == 0         = first
   |  x < (xs !! mid)   = upper_bound_ x xs first half
   | otherwise          = upper_bound_ x xs (mid + 1) (count-half - 1)
  where half = count `div` 2;  mid = first + half

upper_bound x xs  = upper_bound_ x xs 0 (length xs)

-- | equal_range
-- | 
-- | Finds the largest subrange in which x could be inserted in any
-- | place without violating the ordering.

equal_range :: (Ord a) => a -> [a] -> (Int, Int)
equal_range x xs = (lower_bound x xs, upper_bound x xs)

-- | bound
-- | 
-- | Error if x is outside of the domain otherwise find indicies i, j
-- | such that xs!!i <= x <= xs!!(i+1).

bound x xs
  | left == count   = error ("bound :" ++ show x ++ " left of domain")
  | right == 0      = error ("bound :" ++ show x ++ " right of domain")
  | right == count  = (left, right-1)
  | left == right   = (left-1, right)
  | otherwise       =   (left, right)
  where count = length(xs); (left, right) = equal_range x xs

-- | linear_interpolation
-- | 
-- | Error if x is outside of the domain otherwise linearly interpolate.

linear_interpolation :: (Floating a, Ord a) => a -> [a] -> [a] -> a
linear_interpolation x xs ys = 
    let 
      (i', i)   = bound x xs
      (xi', xi) = (xs!!i', xs!!i)
      (yi', yi) = (ys!!i', ys!!i)
      r         = (x - xi)/(xi' - xi)
    in  r*(yi' - yi) + yi

-- | loglinear_interpolation
-- | 
-- | Error if x is outside of the domain otherwise log-linearly interpolate.

loglinear_interpolation :: (Floating a, Ord a) => a -> [a] -> [a] -> a
loglinear_interpolation x xs ys = 
    let 
      (i', i)   = bound x xs
      (xi', xi) = (xs!!i', xs!!i)
      (yi', yi) = (ys!!i', ys!!i)
      r         = (x - xi)/(xi' - xi)
      lyi       = log yi
      lyi'      = log yi'
    in exp(r*(lyi' - lyi) + lyi)
