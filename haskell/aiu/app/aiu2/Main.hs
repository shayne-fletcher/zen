module Main where

data Lit
  = StrLit String
  | IntLit Int
  | Ident String
  deriving (Show, Eq)

data ExprF a
 = Index a a
 | Call a [a]
 | Unary String a
 | Binary a String a
 | Paren a
 | Literal Lit
 deriving (Show, Eq, Functor)

newtype Fix f = In { out :: f (Fix f) }

inj :: Functor f => f (Fix f) -> Fix f
inj = In

proj :: Functor f => Fix f -> f (Fix f)
proj = out

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Fix f -> a
cata f = f . fmap (cata f) . proj

type CoAlgebra f a = a -> f a

ana :: Functor f => CoAlgebra f a -> a -> Fix f
ana f = inj . fmap (ana f) . f

bottomUp' :: Functor f => (Fix f -> Fix f) -> Fix f -> Fix f
bottomUp' f = cata (f . inj)

topDown :: Functor f => (Fix f -> Fix f) -> Fix f -> Fix f
topDown f = inj . fmap (topDown f) . proj . f

countNodes :: Algebra ExprF Int
countNodes (Index it i) = it + i + 1
countNodes (Call e args) = e + sum args + 1
countNodes (Unary _ arg) = arg + 1
countNodes (Binary l _ r) = l + r + 1
countNodes (Paren arg) = arg + 1
countNodes (Literal _) = 1

type Expr' = Fix ExprF
instance Show Expr' where
   show x = "(" ++ show (proj x) ++ ")"

num :: Int -> Expr'
num n  = inj (Literal (IntLit n))

paren :: Expr' -> Expr'
paren e = inj (Paren e)

plus :: Expr' -> Expr' -> Expr'
plus a b = inj (Binary a "+" b)

main :: IO ()
main = do
  let expr1 = plus (plus (num 1) (num 2)) (num 3)
  putStrLn $ show expr1
  putStrLn $ show (cata countNodes expr1)
