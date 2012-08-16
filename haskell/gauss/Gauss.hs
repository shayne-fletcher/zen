{- | Algorithms based on the Gauss Elimination Method. -}

module Gauss (
  gaussianElimination
, gaussianEliminationWithScaledRowPivoting) where

import List (findIndex)
import Maybe (fromJust)

eliminationStep k m = 
  (take (k + 1) m) ++ 
  [elimination (m!!k) (m!!i) k | i <- [k+1..(numRows-1)]]
  where
    (numCols, numRows) = (length (head m), length m)
    elimination = 
      \ pr tr k ->
        let lam = tr!!k / pr!!k in
          (take k tr) ++ [tr!!j - (lam*pr!!j) | j <- [k..(numCols-1)]]

eliminationPhase' k m =
  if k == ((length m) - 1)
    then m
    else eliminationPhase' (k + 1) (eliminationStep k m) 

-- Perform the elimination phase on the augmented coefficient matrix.
eliminationPhase = eliminationPhase' 0

backSubstitutionStep k m x =
  (bk - (sum [((m!!k)!!j)*x!!j | j <- [(k+1)..(numCols-2)]] ))/akk
  where bk      = ((m!!k)!!(numCols-1))
        akk     = (m!!k)!!k
        numCols = length (head m)

backSubstitutionPhase' k m x
   | k < 0      = x
   | otherwise  = backSubstitutionPhase' (k-1) m x'
   where xk            = backSubstitutionStep k m x
         (left, right) = splitAt k x
         x'            = (left ++ [xk] ++ (drop 1 right))

-- Perform the back substituion phase on the upper diagonal augmented
-- coefficient matrix.
backSubstitutionPhase m = 
  backSubstitutionPhase' (n-1) m x
  where  x = replicate n 0.0; n = length m

-- Construct an augmented coefficient matrix from (square) coefficient
-- matrix 'a' and right hand side 'b'.
augmentedCoeffMatrix a b = [a!!i ++ [b!!i] | i <- [0..((length a) - 1)]]

{- | Solve the equation Ax = b using Gaussian elimination.
 
     For example, if     

@
          4.0  -2.0   1.0
     A = -2.0   4.0  -2.0
          1.0  -2.0   4.0
@

     and @b = [11.0, -16.0, 17.0]'@ then, the expected solution
     vector x is @[1.0, -2.0, 3.0]'@.
-}
gaussianElimination :: 
   [[Double]] -- ^ Coefficient matrix 'A'
 -> [Double] -- ^ Right hand side column vector 'b'
 -> [Double] -- ^ Returns solution vector 'x'
gaussianElimination a b 
  | null a
     || ((length a) /= (length b))
     || or [(length row) /= (length b) | row <- a] = 
        error "invalid matrix/vector shape for Gauss elminiation method."
  | otherwise = 
     (backSubstitutionPhase . eliminationPhase) (augmentedCoeffMatrix a b)

-- Compute the scale factor of each row defined as 
-- s[i] = max (|A[i,j]|, i < n and j < n).
scaleFactors' i s m
  | i == numRows = s
  | otherwise = (take i s)         ++ 
                [scaleFactor m i]  ++ 
                (scaleFactors' (i+1) s m)
  where scaleFactor = 
         \m i -> maximum [abs x | x <- m!!i]
        numRows = length m
scaleFactors = scaleFactors' 0 []

--  Find the row p with r[p, k] = max (r[j, k], j >= k) where 
--  r[j,k] = |A[j, k]|/s[j].
bestPivot m k s = 
  if isSingular m k p 1.0e-10
    then error "singular matrix in Gauss elimination method"
    else p
  where 
    p = k + fromJust (findIndex (== best) r)
    best = maximum r
    r = [(abs ((m!!j)!!k))/s!!k | j <- [k..numRows-1]]
    numRows = length m
    isSingular = \m k p tol -> (abs ((m!!p)!!k)) < tol

--  Swap rows k and p in m. 
swapRows m k p
   | k == p = m 
   | otherwise = 
         (take k m) ++ [m!!p] ++ 
         (take (p-(k+1)) (drop (k+1) m)) ++ [m!!k] ++ 
         (drop (p+1) m)

-- Perform the elimination with scaled row pivoting phase on the
-- augmented coefficient matrix.
eliminationPhaseWithPivot' k m s
  | k == (numRows - 1) = m
  | otherwise = 
       eliminationPhaseWithPivot' (k + 1) (eliminationStep k m') s'
       where p        = bestPivot m k s
             (m', s') = (swapRows m k p, swapRows s k p)
             numRows = length m
eliminationPhaseWithPivot = eliminationPhaseWithPivot' 0

{- | Solve the equation Ax = b using Gaussian elimination with scaled
     row pivoting.  

     For example, if

@
           2.0  -2.0   6.0
      A = -2.0   4.0   3.0
          -1.0  -8.0   0.0
@

   and @b = [61.0, 0.0, -1.0]'@ then, the expected solution vector
   x is @[1.0, -1.0, 2.0]'@.
-}
gaussianEliminationWithScaledRowPivoting
 ::  [[Double]] -- ^ Coefficient matrix 'A'
   -> [Double]  -- ^ Right hand side column vector 'b'
   -> [Double]  -- ^ Returns solution vector 'x'
gaussianEliminationWithScaledRowPivoting a b
  | null a
     || ((length a) /= (length b))
     || or [(length row) /= (length b) | row <- a] = 
        error "invalid matrix/vector shape for Gauss elminiation method."
  | otherwise = 
      backSubstitutionPhase 
       (eliminationPhaseWithPivot (augmentedCoeffMatrix a b) (scaleFactors a))

