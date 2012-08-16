module Roots (bisect, newton) where

bisect_ f a b tol its max_its
  | its >= max_its      = error "bisect: Max iterations"
  | a > b               = error "bisect: Root not bracketed"
  | fa == 0             = (a, a, its)
  | fb == 0             = (b, b, its)
  | abs (a - b) < tol   = (a, c, its)
  | fa*fc < 0           = bisect_ f a c tol (its+1) max_its
  | otherwise           = bisect_ f c b tol (its+1) max_its
  where fa = f a; fb  = f b; fc = f c; c = (a + b)/2

bisect f a b tol max_its = bisect_ f a b tol 0 max_its

newton_ f x tol its max_its
  | its >= max_its = error "newton: Max iterations"
  | (abs fx) < tol = (x, its)
  | otherwise      = newton_ f (x - fx/fx') tol (its+1) max_its
  where (fx, fx')  = f x

newton f x tol max_its = newton_ f x tol 0 max_its
