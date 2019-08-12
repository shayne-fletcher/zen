
partition :: [a] -> [[[a]]]
partition (h : tl) = concatMap (extend h) (partition tl)
  where
    extend :: a -> [[a]] -> [[[a]]]
    extend x [] = [[[x]]]
    extend x (h : tl) = ((x : h) : tl) : map (h :) (extend x tl)
partition [] = [[]]

main :: IO ()
main = do
  putStrLn $ show (partition [1, 2, 3])
