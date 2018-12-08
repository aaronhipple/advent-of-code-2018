module Advent05
  ( exec_05
  , react
  ) where

import           Data.Char
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           Data.Semigroup     (Arg (..), Min (..), getMin, sconcat)
import qualified Data.Set           as Set

exec_05 :: IO ()
exec_05 = do
  putStrLn "*********"
  putStrLn "* Day 5 *"
  putStrLn "*********"
  polymer <- head . lines <$> readFile "resources/05.txt"
  putStrLn $ "Units remaining: " ++ show (length (react polymer))
  putStrLn $ "Minimum by dropping: " ++ show (minLength polymer)

alpha = 'a' :| "bcdefghijklmnopqrstuvwxyz"

minLength :: String -> (Char, Int)
minLength polymer = (c, l)
  where
    Arg l c = getMin $ sconcat $ NonEmpty.map f alpha
    f :: Char -> Min (Arg Int Char)
    f c = Min (Arg (length $ react $ filter ((/= c) . toLower) polymer) c)

react :: String -> String
react cs =
  fst $ head $ dropWhile (uncurry (/=)) $ zip iterations (tail iterations)
  where
    iterations :: [String]
    iterations = iterate reactOnce cs

reactOnce :: String -> String
reactOnce [] = []
reactOnce [c] = [c]
reactOnce (c:c':cs)
  | c == swapCase c' = reactOnce cs
  | otherwise = c : reactOnce (c' : cs)

swapCase :: Char -> Char
swapCase c
  | isUpper c = toLower c
  | isLower c = toUpper c
  | otherwise = c
