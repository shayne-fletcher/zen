import Data.Bits

data Tree = Leaf Bool | Node Tree Tree Bool
  deriving Show

getMark :: Tree -> Bool
getMark (Leaf m) = m
getMark (Node _ _ m) = m

data Ctx = Top | L Ctx Tree | R Tree Ctx
  deriving Show

type Loc = (Tree, Ctx)

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
make h = make_rec h
  where
    make_rec :: Int -> Tree
    make_rec level =
      case level of
        0 -> Leaf False
        _ -> Node (make_rec (level - 1)) (make_rec (level - 1)) False

path :: Int -> Int -> (Tree -> Loc)
path n h = path_rec top n 0
  where
    path_rec :: (Tree -> Loc) -> Int -> Int -> (Tree -> Loc)
    path_rec acc n i =
      if i == h then acc else path_rec (dir n i h . acc) n (i + 1)
    -- In `dir x i n`, `x` has length `n` bits, `i` is the bit
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

main = do
  let h = 2
  putStrLn $ show (fst (upmost (mark $ path 3 h $ make h)))

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
