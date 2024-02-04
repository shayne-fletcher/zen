module Main where

import Data.List
import Data.List.Extra (split)

main :: IO ()
main = do
  let toks = split (== '/') "../../../../security/flare/if/impl.rs"
      mapped_path = intercalate "/" $ reverse . snd $ foldl' f z toks
  putStrLn $ show mapped_path
  where
    z :: (Int, [String])
    z = (1, [])

    f :: (Int, [String]) -> String -> (Int, [String])
    f (i, cs) e = case e of
      ".." -> (i + 1, ('d' : show i) : cs)
      _ -> (i, cs)
