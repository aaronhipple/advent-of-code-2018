module Advent12
  ( exec_12
  , loadEntries
  , parsePlants
  , spreader
  , tick
  , score
  ) where

import           Control.Monad                (forM_)
import           Data.List                    (dropWhileEnd, zip3, zip5)
import           Data.Map                     (Map, (!?))
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust, fromMaybe)
import           Debug.Trace
import           Text.ParserCombinators.ReadP
import           Utils                        (parseMaybe)

exec_12 :: IO ()
exec_12 = do
  putStrLn "*********"
  putStrLn "* Day 12 *"
  putStrLn "*********"
  (current, spreads) <- loadEntries
  putStrLn $ "current: " ++ show current
  putStrLn $ "spreads: " ++ show spreads
  putStrLn $ "length: " ++ show current
  putStrLn $ "iterated 20x..."
  let generations = iterate (tick $ spreader spreads) current
  putStrLn $
    unlines $
    map show $ take 21 $ zip3 [0 ..] generations (map score generations)
  -- Getting the score directly at 50G iterations probably not feasible...
  -- let target = 50000000000
  -- putStrLn $ "score at " ++ show target ++ "..."
  -- putStrLn $
  --   "score: " ++ show (score target (generations !! fromIntegral target))
  -- Let's try to detect a cycle instead
  let limit = 10000
  let cycleResult = (findCycle limit generations)
  case cycleResult of
    Nothing -> putStrLn $ "No cycle found in " ++ show limit ++ " generations."
    Just (g1, s1, s2) ->
      putStrLn $
      unlines
        [ "Cycle found at " ++ show g1 ++ " generations"
        , "Gen " ++ show g1 ++ ": " ++ show (s1, score s1)
        , "Gen " ++ show (g1 + 1) ++ ": " ++ show (s2, score s2)
        ]
  -- Turns out it cycles as 100 generations
  -- f 100 = 8000
  -- f 101 = 8080
  -- f x = 80 * x
  -- f 50000000000 = 4000000000000

findCycle :: Int -> [State] -> Maybe (Int, State, State)
findCycle limit generations = findCycle $ take limit $ npairs
  where
    npairs = zip [0 ..] generations
    isCycling :: State -> State -> Bool
    isCycling (State (_, c1)) (State (_, c2)) = c1 == c2
    findCycle :: [(Int, State)] -> Maybe (Int, State, State)
    findCycle [] = Nothing
    findCycle [_] = Nothing
    findCycle ((g1, s1):(g2, s2):xs)
      | isCycling s1 s2 = Just (g1, s1, s2)
      | otherwise = findCycle ((g2, s2) : xs)

data Cell
  = Live
  | Dead
  deriving (Eq)

instance Ord Cell where
  (<=) a b =
    if a == Live
      then True
      else False

instance Show Cell where
  show Live = "#"
  show Dead = "."

newtype State =
  State (Int, [Cell])
  deriving (Eq)

instance Show State where
  show (State (left, cells)) = show left ++ " - " ++ concatMap show cells

newtype Spread =
  Spread ((Cell, Cell, Cell, Cell, Cell), Cell)

instance Show Spread where
  show (Spread ((c1, c2, c3, c4, c5), result)) =
    concatMap show [c1, c2, c3, c4, c5] ++ " => " ++ (show result)

score :: State -> Integer
score (State (left, cs)) =
  sum $
  map (fromIntegral . fst) $
  filter ((== Live) . snd) $ zip [left,left + 1 ..] cs

tick :: ((Cell, Cell, Cell, Cell, Cell) -> Cell) -> State -> State
tick spread (State (left', cs')) = State (left, dropWhileEnd (== Dead) new)
  where
    (dropped, new) = span (== Dead) $ map spread ins
    left = (length dropped) + left' - 1
    ins = zip5 (drop 0 cs) (drop 1 cs) (drop 2 cs) (drop 3 cs) (drop 4 cs)
    cs = pad ++ cs' ++ pad
    pad = replicate 3 Dead

spreader :: [Spread] -> (Cell, Cell, Cell, Cell, Cell) -> Cell
spreader spreads this = fromMaybe Dead $ spreadMap !? this
  where
    spreadMap :: Map (Cell, Cell, Cell, Cell, Cell) Cell
    spreadMap = Map.fromList $ map (\(Spread (k, v)) -> (k, v)) spreads

loadEntries :: IO ((State, [Spread]))
loadEntries = do
  (current, spreads) <-
    fmap (fromJust . parsePlants) (readFile "resources/12.txt")
  return (current, spreads)

parsePlants :: String -> Maybe (State, [Spread])
parsePlants = parseMaybe parser
  where
    parser :: ReadP (State, [Spread])
    parser = do
      state <- parseState
      skipSpaces
      spreads <- many1 parseSpread
      eof
      return (state, spreads)
    parseState :: ReadP (State)
    parseState = do
      string "initial state: "
      cells <- fmap (map toCell) (many1 (choice [char '.', char '#']))
      return (State (0, cells))
    parseSpread :: ReadP (Spread)
    parseSpread = do
      skipSpaces
      c1 <- fmap toCell (choice [char '.', char '#'])
      c2 <- fmap toCell (choice [char '.', char '#'])
      c3 <- fmap toCell (choice [char '.', char '#'])
      c4 <- fmap toCell (choice [char '.', char '#'])
      c5 <- fmap toCell (choice [char '.', char '#'])
      string " => "
      out <- fmap toCell (choice [char '.', char '#'])
      skipSpaces
      return (Spread ((c1, c2, c3, c4, c5), out))
    toCell :: Char -> Cell
    toCell '.' = Dead
    toCell '#' = Live
