module Advent07
  ( exec_07
  , sortInstructions
  , findFirsts
  , findLasts
  , makeGraph
  , Graph(..)
  , trav
  , allNodes
  , allMet
  , runTicks
  ) where

import           Data.Char
import           Data.List                    (foldl', isSubsequenceOf,
                                               partition, sort)
import           Data.Map.Strict              (Map (..))
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (catMaybes, isJust, mapMaybe)
import           Data.Set                     (Set (..))
import qualified Data.Set                     as Set
import           Debug.Trace
import           Text.ParserCombinators.ReadP
import           Utils                        (parseMaybe)

exec_07 :: IO ()
exec_07 = do
  putStrLn "*********"
  putStrLn "* Day 7 *"
  putStrLn "*********"
  graph <- makeGraph . lines <$> readFile "resources/07.txt"
  putStrLn $ "Nodes: " ++ Set.toList (allNodes graph)
  putStrLn $ "First nodes: " ++ Set.toList (findFirsts graph)
  putStrLn $ "Last nodes: " ++ Set.toList (findLasts graph)
  putStrLn $ "Traversed (Pt 1): " ++ (show $ trav graph)
  putStrLn "* Part 7 *"
  putStrLn $ "Time: " ++ (show $ snd $ runTicks 5 60 graph)

newtype Graph a =
  Graph [Edge a]
  deriving (Show, Eq)

type Edge a = (a, a)

sortInstructions :: [String] -> String
sortInstructions instructions = trav graph
  where
    graph = makeGraph instructions

makeGraph :: [String] -> Graph Char
makeGraph = Graph . mapMaybe parseInstruction

trav :: (Show a, Eq a, Ord a) => Graph a -> [a]
trav g =
  (\(_, _, x) -> x) $
  last $ take (1 + Set.size nodes) $ iterate f (queue', indegrees', [])
  where
    nodes = allNodes g
    edges = getEdges g
    indegrees' = Map.fromSet (\v -> length $ filter (isTailOf v) edges) nodes
    queue' = Map.keysSet (Map.filter (== 0) indegrees')
    f (queue, indegrees, visited) = (newqs, newids, visited ++ [v])
      where
        Just (v, qs) = Set.minView queue
        heads = map snd $ filter (isHeadOf v) edges
        newids =
          Map.unionWith (-) indegrees (Map.fromList (zip heads (repeat 1)))
        newqs =
          Set.union qs $
          Set.filter
            ((flip Set.notMember) (Set.union (Set.fromList visited) queue))
            (Map.keysSet (Map.filter (== 0) newids))

tick ::
     (Show a, Eq a, Ord a)
  => Int
  -> Int
  -> Graph a
  -> (Map a Int, ([a], Int))
  -> (Map a Int, ([a], Int))
tick num base g (pending, (done, ct)) = (pending'', (done', ct + 1))
  where
    pending' = Map.filter (> 0) $ Map.map ((flip (-)) 1) pending
    canStart =
      Set.filter
        (allMet g (Set.fromList done'))
        (Set.difference
           (allNodes g)
           (Set.union (Map.keysSet pending') (Set.fromList done')))
    pending'' =
      Map.union pending' $
      Map.take
        (num - Map.size pending')
        (Map.fromSet (initTime base g) canStart)
    done' = done ++ Map.keys (Map.difference pending pending')

runTicks num base g =
  head $
  dropWhile ((/= Map.empty) . fst) $
  take limit $ iterate (tick num base g) initial
  where
    limit = 100000
    initial =
      (Map.fromSet (initTime base g) $ Set.take num (findFirsts g), ([], 0))

initTime :: (Show a, Ord a, Eq a) => Int -> Graph a -> a -> Int
initTime base g v = base + 1 + Set.findIndex v nodes
  where
    nodes = allNodes g

allMet :: Ord a => Graph a -> Set a -> a -> Bool
allMet (Graph edges) seen v = all ((flip Set.member) seen) dependencies
  where
    dependencies = map fst $ filter (isTailOf v) edges

isHeadOf :: Eq a => a -> Edge a -> Bool
isHeadOf v e = fst e == v

isTailOf :: Eq a => a -> Edge a -> Bool
isTailOf v e = snd e == v

getEdges :: Graph a -> [Edge a]
getEdges (Graph edges) = edges

findFirsts :: (Ord a, Eq a) => Graph a -> Set a
findFirsts (Graph edges) = Set.filter (not . isHead) nodes
  where
    heads = map snd edges
    isHead v = v `elem` heads
    nodes = allNodes (Graph edges)

findLasts :: (Ord a, Eq a) => Graph a -> Set a
findLasts (Graph edges) = Set.filter (not . isTail) nodes
  where
    tails = map fst edges
    isTail v = v `elem` tails
    nodes = allNodes (Graph edges)

allNodes :: Ord a => Graph a -> Set a
allNodes (Graph edges) = Set.fromList $ uncurry (++) $ unzip edges

parseInstruction :: String -> Maybe (Edge Char)
parseInstruction = parseMaybe parser
  where
    parser = do
      string "Step "
      upstream <- satisfy isAlpha
      string " must be finished before step "
      downstream <- satisfy isAlpha
      string " can begin."
      return $ (upstream, downstream)
