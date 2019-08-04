
partition :: [a] -> [[[a]]]
partition [] = [[]]
partition (h : tl) = concatMap (thread h) (partition tl)

thread :: a -> [[a]] -> [[[a]]]
thread x [] = [[[x]]]
thread x (h : tl) = ((x : h) : tl) : map (h :) (thread x tl)

main :: IO ()
main = do
  putStrLn $ show (partition [1, 2, 3])
  putStrLn $ show (thread 1 [[2, 3]])
  putStrLn $ show (thread 1 [[3], [2]])
