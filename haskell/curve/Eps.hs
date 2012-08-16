module Eps (eps) where

eps_ x  
  | (x/2) + 1 > 1 = eps_ (x/2)
  | otherwise = x

eps = eps_ 1 -- e.g. 2.220446049250313e-16
