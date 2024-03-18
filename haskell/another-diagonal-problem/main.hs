{-# LANGUAGE ViewPatterns #-}

import Data.Foldable(traverse_)

diag :: [[a]] -> [[a]]
diag [] = []
diag [r] = map (:[]) r
diag (r : (diag -> ds)) = zipWith (:) r ([]:ds) ++ drop (length r - 1) ds

main :: IO ()
main = do
 let m = [
         [1, 2, 3]
       , [4, 5, 6]
       , [7, 8, 9]
       ]
 traverse_ print (diag m)
