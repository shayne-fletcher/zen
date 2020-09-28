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


-- e.g.
--   map3 f (x0 : ys0) (x1 : ys1) (x2 : ys2) = f x0 x1 x2 : map3 f ys0 ys1 ys2
mapN :: Int -> Q [Dec]
mapN n
  | n >= 1    = (:[]) <$> (funD name [cl1, cl2])
  | otherwise = fail "mapN: argument n may not be <= 0."
  where
    name = mkName $ "map" ++ show n
    cl1  = do f  <- newName "f"
              xs <- replicateM n (newName "x")
              ys <- replicateM n (newName "ys")
              let argPatts  = varP f : consPatts
                  consPatts = [ [p| $(varP x) : $(varP ys) |]
                              | (x,ys) <- xs `zip` ys ]
                  apply     = foldl (\ g x -> [| $g $(varE x) |])
                  first     = apply (varE f) xs
                  rest      = apply (varE name) (f:ys)
              clause argPatts (normalB [| $first : $rest |]) []
    cl2  = clause (replicate (n+1) wildP) (normalB (conE '[])) []
