module Advent12
  ( exec_12
  ) where

import           Data.Maybe                   (fromJust)
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

data Cell
  = Live
  | Dead

instance Show Cell where
  show Live = "#"
  show Dead = "."

newtype State =
  State [Cell]

instance Show State where
  show (State cells) = concatMap show cells

newtype Spread =
  Spread ((Cell, Cell, Cell, Cell, Cell), Cell)

instance Show Spread where
  show (Spread ((c1, c2, c3, c4, c5), result)) =
    concatMap show [c1, c2, c3, c4, c5] ++ " => " ++ (show result)

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
      return (State cells)
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
