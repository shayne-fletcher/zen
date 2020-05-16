{-# LANGUAGE TypeApplications #-}

module Main where

import System.Environment
import Control.Monad
import Data.List

units :: [String]
units =
  [ ""
  , "thousand"
  , "million"
  , "billion"
  , "trillion"
  ]

split :: Int -> [(Int, String)]
split n = reverse (zip (go n) units)
  where
    go n
      | n >= 10^15 = error "Max value 999 trillion"
      | n <= 999 = [n]
      | otherwise = n `mod` 1000 : (go $ n `div` 1000)

ones :: Int -> String
ones n
  -- | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | otherwise = error "Impossible"

teens :: Int -> String
teens n
  | n == 10 = "ten"
  | n == 11 = "eleven"
  | n == 12 = "twelve"
  | n == 13 = "thirteen"
  | n == 14 = "fourteen"
  | n == 15 = "fifteen"
  | n == 16 = "sixteen"
  | n == 17 = "seveteen"
  | n == 18 = "eighteen"
  | n == 19 = "nineteen"
  | otherwise = error "Impossible"

tens :: Int -> String
tens n
  | n == 2 = "twenty"
  | n == 3 = "thirty"
  | n == 4 = "fourty"
  | n == 5 = "fifty"
  | n == 6 = "sixty"
  | n == 7 = "seventy"
  | n == 8 = "eighty"
  | n == 9 = "ninety"
  | otherwise = error "Impossible"

toWords n
  | n == 0 = "zero"
  | n < 10 = ones n
  | n < 20 = teens n
  | n < 100 = tens (n `div` 10) ++ test (n `mod` 10) ones
  | n < 1000 = ones (n `div` 100) ++ " hundred " ++ test (n `mod` 100) toWords
  | otherwise = intercalate " " [ test amt $ (++ (" " ++ unit)) . toWords | (amt, unit) <- split n ]
  where test rem f = if rem > 0 then f rem else ""

main = do
  args <- getArgs
  when (length args == 1) $ do
    let num = read @Int $ head args
    putStrLn $ toWords num
  pure ()
