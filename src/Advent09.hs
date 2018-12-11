module Advent09
  ( exec_09
  , newGame
  , turn
  , GameState(..)
  , atTurn
  ) where

import           Data.Map.Strict (Map (..))
import qualified Data.Map.Strict as Map
import           Data.Sequence   (Seq (..), (><))
import qualified Data.Sequence   as S

exec_09 :: IO ()
exec_09 = do
  putStrLn "*********"
  putStrLn "* Day 9 *"
  putStrLn "*********"
  putStrLn $
    "Score after 7151000 turns: " ++
    (show $ foldl max 0 (scores $ atTurn 7151000 $ iterate turn (newGame 447)))

data GameState = GameState
  { marble      :: Int
  , marbles     :: Seq Int
  , marbleCount :: Int
  , player      :: Int
  , players     :: Int
  , scores      :: Map Int Int
  , index       :: Int
  } deriving (Eq, Show)

newGame :: Int -> GameState
newGame players =
  GameState
  { marble = 0
  , marbles = S.singleton 0
  , marbleCount = 1
  , player = 0
  , players = players
  , scores = scores
  , index = 1
  }
  where
    scores = Map.fromList $ zip [1 .. players] (repeat 0)

atTurn :: Int -> [x] -> x
atTurn i xs = head $ drop i xs

turn :: GameState -> GameState
turn (GameState m ms ct p ps scores i) =
  GameState
  { marble = m'
  , marbles = ms'
  , marbleCount = ct'
  , player = p'
  , players = ps
  , scores = scores'
  , index = i'
  }
  where
    m' = m + 1
    p'
      | (p + 1) <= ps = p + 1
      | otherwise = 1
    mod23 = m' `mod` 23
    isScoring = mod23 == 0
    willScore = mod23 == 22
    ct'
      | isScoring = ct - 1
      | otherwise = ct + 1
    i'
      | willScore = scoreMod
      | nextMod == 0 = ct'
      | otherwise = nextMod
      where
        scoreMod = (i - 7) `mod` ct'
        nextMod = (i + 2) `mod` ct'
    scores'
      | isScoring = Map.adjust (+ (m' + (ms `S.index` i))) p' scores
      | otherwise = scores
    ms'
      | isScoring = dropAt i ms
      | otherwise = insertAt i ms m'

dropAt :: Show a => Int -> Seq a -> Seq a
dropAt i xs = as >< S.drop 1 bs
  where
    (as, bs) = S.splitAt i xs

insertAt :: Int -> Seq a -> a -> Seq a
insertAt i xs x = as >< S.singleton x >< bs
  where
    (as, bs) = S.splitAt i xs
