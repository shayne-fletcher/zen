module Main where

data Lit
  = StrLit String
  | IntLit Int
  | Ident String
  deriving (Show, Eq)

-- data Expr
--   = Index Expr Expr
--   | Call Expr [Expr]
--   | Unary String Expr
--   | Binary Expr String Expr
--   | Paren Expr
--   | Literal Lit
--   deriving (Show, Eq)

-- data Stmt
--   = Break
--   | Continue
--   | Empty
--   | IfElse Expr [Stmt] [Stmt]
--   | Return (Maybe Expr)
--   | While Expr [Stmt]
--   | Expression Expr
--   deriving (Show, Eq)

-- applyExpr :: (Expr -> Expr) -> Expr -> Expr
-- applyExpr _ (Literal i)  = Literal i
-- applyExpr f (Paren e)  = Paren (f e)
-- applyExpr f (Index e i) = Index (f e) (f i)
-- applyExpr f (Call e args) = Call (f e) (map f args)
-- applyExpr f (Unary op e) = Unary op (f e)
-- applyExpr f (Binary l op r) = Binary (f l) op (f r)

-- flatten' :: Expr -> Expr
-- flatten' (Paren e) = flatten' e
-- flatten' e =  applyExpr flatten' e

data ExprF a
 = Index a a
 | Call a [a]
 | Unary String a
 | Binary a String a
 | Paren a
 | Literal Lit
 deriving (Show, Eq, Functor)

-- apply :: (a  -> b) -> ExprF a -> ExprF b
-- apply _ (Literal i) = Literal i
-- apply f (Paren e)  = Paren (f e)
-- apply f (Index e i) = Index (f e) (f i)
-- apply f (Call e args) = Call (f e) (map f args)
-- apply f (Unary op e) = Unary op (f e)
-- apply f (Binary l op r) = Binary (f l) op (f r)

-- If we require that the children of an expression be expressions we are left to conclude that `a = ExprF (ExperF (ExprF ...))`.
-- How do we represent this sequence finitely?

-- We need a datatype Y, that when given a datatype f, wraps an f whose children are of type Y f. Call it Term

-- data Term f = In (f (Term f))

newtype Term f = In { out :: f (Term f) }

-- Note that In :: f (Term f) -> Term f. That is, we have to pass an ExprF that, if it has children, have type Term ExprF.

type Expr' = Term ExprF
instance Show (Term ExprF) where
   show x = "(" ++ show (out x) ++ ")"

one :: Expr'
one = In (Literal (IntLit 1))

two :: Expr'
two = In ((Binary one "+" (In (Paren one))) :: ExprF (Term ExprF))

-- In @ExprF :: ExprF (Term ExprF) -> Term ExprF

-- If we want to end up with a `Term ExprF`, we have to pass an `ExprF` that, if it has children, contains further values of `Term ExprF`.

bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp f = f . In . fmap (bottomUp f) . out

topDown :: Functor a => (Term a -> Term a) -> Term a -> Term a
topDown f = In . fmap (topDown f) . out . f

flattenTerm :: Expr' -> Expr'
flattenTerm (In (Paren e)) = e
flattenTerm other = other

flatten :: Expr' -> Expr'
flatten = bottomUp flattenTerm

flatten' :: Expr' -> Expr'
flatten' = topDown flattenTerm

main :: IO ()
main = do
  putStrLn $ show one
  putStrLn $ show two
  putStrLn $ show (flatten two)
  putStrLn $ show (flatten' two)
