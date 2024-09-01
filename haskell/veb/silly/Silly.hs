import Data.Bits
import Debug.Trace

data Tree = Leaf Bool | Node Tree Tree Bool
  deriving (Show)

-- Huet zipper http://www.st.cs.uni-sb.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf
data Ctx = Top | L Ctx Tree | R Tree Ctx
  deriving (Show)

type Loc = (Tree, Ctx)

getMark :: Tree -> Bool
getMark (Leaf m) = m
getMark (Node _ _ m) = m

left :: Loc -> Loc
left (Node l r _, c) = (l, L c r)

right :: Loc -> Loc
right (Node l r _, c) = (r, R l c)

top :: Tree -> Loc
top t = (t, Top)

up :: Loc -> Loc
up (t, L c r) = (Node t r (getMark t || getMark r), c)
up (t, R l c) = (Node l t (getMark l || getMark t), c)

upmost :: Loc -> Loc
upmost l@(t, Top) = l
upmost l = upmost (up l)

modify :: Loc -> (Tree -> Tree) -> Loc
modify (t, c) f = (f t, c)

mark :: Loc -> Loc
mark (Leaf _, c) = (Leaf True, c)
mark (Node l r _, c) = (Node l r True, c)

make :: Int -> Tree
make h =
  let (t, num_nodes) = make_rec h
   in Debug.Trace.trace ("tree of " <> show num_nodes <> " nodes") t
  where
    make_rec :: Int -> (Tree, Int)
    make_rec level =
      case level of
        0 -> (Leaf False, 1)
        _ ->
          let (l, c1) = make_rec (level - 1)
              (r, c2) = make_rec (level - 1)
           in (Node l r False, 1 + c1 + c2)

path :: Int -> Int -> (Tree -> Loc)
path n h = path_rec top n 0
  where
    path_rec :: (Tree -> Loc) -> Int -> Int -> (Tree -> Loc)
    path_rec acc n i =
      if i == h then acc else path_rec (dir n i h . acc) n (i + 1)
    -- In `dir x i n`, `x` has length `n` bits, `i` is the bit under
    -- consideration. e.g. when n = 4, x can represent values 0..15.
    -- If x = 13 (0b1101) say, we compute `right . right . left .
    -- right`
    dir :: Int -> Int -> Int -> (Loc -> Loc)
    dir x i n =
      case (x `shiftL` i .&. mask) `shiftR` k of
        0 -> left
        1 -> right
      where
        k = n - 1
        mask = 1 `shiftL` k

fold :: (a -> Tree -> a) -> a -> Tree -> a
fold f acc n@(Node l r _) =
  let acc' = fold f acc l
      acc'' = fold f acc' r
   in f acc'' n
fold f acc n@(Leaf _) = f acc n

main = do
  let h = 16
      t = make h
      t' = fst . upmost . mark $ path 3 h t
      leaves = fold f [] t
  putStrLn $ "n = " <> show (length leaves)
  case t of
    Node _ _ True -> putStrLn "root marked"
    Node _ _ False -> putStrLn "root not marked"
    _ -> error "failure"
  where
    f acc n = case n of Node {} -> acc; Leaf {} -> n : acc

{-
{-# LANGUAGE RecordWildCards #-}

import Data.List
import Data.Maybe
import Debug.Trace

data Node = Nil
  | Node {
      parent:: Maybe Node,
      left :: Node,
      right :: Node,
      marked :: Bool
    }
  deriving(Eq)

make :: Int -> Node
make h = make_rec (h + 1) Nothing
  where
    make_rec :: Int -> Maybe Node -> Node
    make_rec level p =
      case level of
        0 -> Nil
        _ ->
          let n =
                Node {
                  parent = p,
                  left = make_rec (level - 1) (Just n),
                  right = make_rec (level - 1) (Just n),
                  marked = False
                }
          in n

fold :: (a -> Node -> a) -> a -> Node -> a
fold f acc n@Node { parent = _, left = l, right = r, marked = _} = f (fold f (fold f acc l) r) n
fold f acc n@Nil = f acc n

numEdges n Node { parent = Nothing } = 0 :: Int
numEdges n Node { parent = Just p } = numEdges n p + 1

main = do
  let k = 4
      t = make k
      leaves = fold f [] t
  putStrLn ("number leaves: " <> show (length leaves))
  putStrLn ("num edges of 15: " <> show (numEdges 0 (leaves!!12)))

  pure ()
  where
    f acc n =
      case n of
        Node { parent=_, left = Nil, right = Nil } -> n : acc
        _ -> acc
-}
