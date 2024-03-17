{-
  1, 2, 3
  4, 5, 6
  7, 8, 9

  1, 2, 3
     4, 5, 6
        7, 8, 9
-}
diag :: [[a]] -> [[a]]
diag [] = []
diag [r] = map (:[]) r
diag (r : rs) = zipWith (:) r ([]:ds) ++ drop (length r - 1) ds
  where ds = diag rs
main = do
 let m = [
         [1, 2, 3]
       , [4, 5, 6]
       , [7, 8, 9]
       ]
 putStrLn$ show (diag m)
