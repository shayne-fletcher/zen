{-# LANGUAGE InstanceSigs #-}

import GHC.Arr

fsm :: Integer -> Char -> Integer
fsm 0 '(' = 1
fsm 0 _   = 0
fsm 1 '0' = 2
fsm 1 _   = 1
fsm 2 '0' = 3
fsm 2 _   = 1
fsm 3 '7' = 4
fsm 3 '0' = 3
fsm 3 _   = 1
fsm 4 ')' = 5
fsm 4 _   = 4
fsm 5 _   = 5

matches :: String -> Bool
matches s = Prelude.foldl fsm 0 s == 5

-- main = do
--   putStrLn $ show (matches "(007)")
--   putStrLn $ show (matches "He(007xxxxxxxxxxxx)llo")

{- tabulated form of fsm:
         0  1  2  3  4  5
    ' '
    '!'
    '"'
     .
     .
    '('  1
    ...
    ')'              5
    ...
    'z'
-}

tabulate :: (Integer -> Integer) -> Array Integer Integer
tabulate f = array (0, 5) [(i, f i) | i <- range (0, 5)]

letters :: Array Char (Array Integer Integer)
letters = array (' ','z') [(i,tabulate (flip fsm i)) | i <- range (' ','z')]

type Table = Array Integer Integer

instance Semigroup Table where
  (<>) :: Table -> Table -> Table
  f <> g =  tabulate (\s -> g ! (f ! s))

instance Monoid Table where
  mempty :: Table
  mempty = tabulate id

main = do
  let p = \i -> (letters ! '(' <> letters ! '0' <> letters ! '0' <> letters ! '7' <> letters ! ')' ) ! i
  putStrLn $ show (p 0)
