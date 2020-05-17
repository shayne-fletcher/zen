{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

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

ones :: Int -> [String]
ones = \case
  1 -> ["one"]
  2 -> ["two"]
  3 -> ["three"]
  4 -> ["four"]
  5 -> ["five"]
  6 -> ["six"]
  7 -> ["seven"]
  8 -> ["eight"]
  9 -> ["nine"]
  _ -> error "Impossible"

teens :: Int -> [String]
teens = \case
  10 -> ["ten"]
  11 -> ["eleven"]
  12 -> ["twelve"]
  13 -> ["thirteen"]
  14 -> ["fourteen"]
  15 -> ["fifteen"]
  16 -> ["sixteen"]
  17 -> ["seveteen"]
  18 -> ["eighteen"]
  19 -> ["nineteen"]
  _ -> error "Impossible"

tens :: Int -> [String]
tens = \case
  2 -> ["twenty"]
  3 -> ["thirty"]
  4 -> ["fourty"]
  5 -> ["fifty"]
  6 -> ["sixty"]
  7 -> ["seventy"]
  8 -> ["eighty"]
  9 -> ["ninety"]
  _ -> error "Impossible"

toWords :: Int -> [String]
toWords n
  | n == 0 = ["zero"]
  | n < 10 = ones n
  | n < 20 = teens n
  | n < 100 = tens (n `div` 10) ++ test (n `mod` 10) ones
  | n < 1000 = ones (n `div` 100) ++ ("hundred" : test (n `mod` 100) toWords)
  | otherwise = concat [ toWords amt ++ [unit] | (amt, unit) <- split n ]
  where
    test rem f = if rem > 0 then f rem else []

main = do
  args <- getArgs
  when (length args == 1) $ do
    let num = read @Int $ head args
    putStrLn $ intercalate " " (toWords num)
  pure ()
