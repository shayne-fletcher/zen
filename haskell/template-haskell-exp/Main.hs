{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import T

$(genCurries 20)

$(mapN 3)

main :: IO ()
main = do
  let f2 (a, b) = a + b
  let f3 (a, b, c) = a + b + c
  let f4 (a, b, c, d) = a + b + c + d
  print $ curry2 f2 1 2
  print $ curry3 f3 1 2 3
  print $ curry4 f4 1 2 3 4
  print $ map3 (\x y z -> x + y + z) [1, 2] [1, 2] [1, 2]
