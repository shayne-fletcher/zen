{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module T where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Lib

curryN :: Int -> Q Exp
curryN n =
  newName "f" >>= \f ->
  replicateM n (newName "x") >>= \xs ->
  let args = map VarP (f : xs)
      ntup = TupE (map (Just . VarE) xs)
  in return $ LamE args (AppE (VarE f) ntup)

genCurries :: Int -> Q [Dec]
genCurries n = forM [1..n] mkCurryDec
  where mkCurryDec i = do
          cury <- curryN i
          let name = mkName $ "curry" ++ show i
          return $ FunD name [Clause [] (NormalB cury) []]

genId :: Q Exp
genId = do
  x <- newName "x"
  lamE [varP x] (varE x)

genId' :: Q Exp
genId' = [| \x -> x |]
