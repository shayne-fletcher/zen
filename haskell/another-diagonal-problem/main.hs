{-# LANGUAGE ViewPatterns #-}

import Data.Foldable(traverse_)

{-
  1, 2, 3
  4, 5, 6
  7, 8, 9

  1 2 3
    4 5 6
      7 8 9

  1 2 3 6 9
    4 5 8
      7

      3
    2 5 6
  1 4 7 8 9

  1
  2, 4
  3, 5, 7
  6, 8
  9

        1
     2, 4
  3, 5, 7
     6, 8
        9


-}
diag :: [[a]] -> [[a]]
diag [] = []
diag [r] = map (:[]) r
diag (r : (diag -> ds)) = zipWith (:) r ([]:ds) ++ [last ds]

main :: IO ()
main = do
 let m = [
         [1, 2, 3]
       , [4, 5, 6]
       , [7, 8, 9]
       ]
 traverse_ print (diag m)
