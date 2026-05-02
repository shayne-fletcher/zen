{-
Tree-based multicast (broadcast) with hierarchical, recursive
partitioning.
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-name-shadowing -Wno-x-partial #-}

import Control.Concurrent
import Control.Monad

import Data.Map.Strict qualified as Map

data NextHop = NextHop {
  offset :: Int,
  dims :: [Int]
  } deriving (Show)

next_hops :: [Int] -> [NextHop]
next_hops shape = next_hops_rec shape (product shape)
  where
    next_hops_rec :: [Int] -> Int -> [NextHop]
    next_hops_rec shape block_len =
      case shape of
        [] -> []
        n : ns ->
          if n <= 1 || block_len <= 1
          then
            next_hops_rec ns block_len
          else
            let len = block_len `div` n in
            map (mkHop len ns) [1 .. n - 1] ++ next_hops_rec ns len
      where
        mkHop :: Int -> [Int] -> Int -> NextHop
        mkHop len ns i = NextHop { offset = i * len, dims = ns }

routing_tree :: forall a. [a] -> [Int] -> [(a, a)]
routing_tree [] _ = []
routing_tree [_] _ = []
routing_tree members shape =
  concatMap edges (next_hops shape)
  where
    root :: a
    root = head members

    edges :: NextHop -> [(a, a)]
    edges hop =
      let start = offset hop
          ns = dims hop
          len = product ns
          childMembers = take len (drop start members)
          childRoot = head childMembers
       in (root, childRoot) : routing_tree childMembers ns

adjacencyList :: Ord a => [(a, a)] -> Map.Map a [a]
adjacencyList = foldr (\(p, c) m -> Map.insertWith (++) p [c] m) Map.empty

proc_ :: String -> Chan String -> [(String, Chan String)] -> IO ()
proc_ name inbox children = forever $ do
  msg <- readChan inbox
  putStrLn $ name ++ " received: " ++ msg
  forM_ children $ \(childName, childInbox) -> do
    putStrLn $ name ++ " forwarding to " ++ childName
    writeChan childInbox msg

main :: IO ()
main = do
  let procs = ["A", "B", "C", "D"]
      shape = [2, 2]
      edges = routing_tree procs shape
      childMap = adjacencyList edges

  putStrLn $ "routing tree: " ++ show edges

  chans <- forM procs $ \m -> do
    ch <- newChan
    pure (m, ch)

  let chanMap = Map.fromList chans

  forM_ procs $ \m -> do
    let inbox = chanMap Map.! m
        childNames = Map.findWithDefault [] m childMap
        childChans = [(c, chanMap Map.! c) | c <- childNames]
    _ <- forkIO $ proc_ m inbox childChans
    pure ()

  putStrLn "casting from A"
  writeChan (chanMap Map.! "A") "hello mesh"

  threadDelay 1000000
