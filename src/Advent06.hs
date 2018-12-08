module Advent06
  ( exec_06
  , distance
  , borderElems
  , trimEdges
  ) where

import           Data.Char                    (isDigit)
import           Data.List.NonEmpty           (fromList)
import           Data.Maybe                   (catMaybes, mapMaybe)
import           Data.Semigroup
import           Data.Set                     (Set (..))
import qualified Data.Set                     as Set
import           Debug.Trace
import           Text.ParserCombinators.ReadP
import           Utils                        (argmins, mostCommonWithCount,
                                               parseMaybe)

exec_06 :: IO ()
exec_06 = do
  putStrLn "*********"
  putStrLn "* Day 6 *"
  putStrLn "*********"
  coords <- mapMaybe parse . lines <$> readFile "resources/06.txt"
  -- print $ mostCommonWithCount $ fromList $ concatMap catMaybes (grid coords)
  putStrLn $
    "Within 10k of all: " ++
    (show $ length $ concatMap catMaybes $ grid2 coords)

printGrid :: [[Maybe Int]] -> String
printGrid gd = unlines $ map (map print) gd
  where
    print Nothing = '.'
    print (Just n)
      | n `mod` 2 == 0 = 'o'
      | n `mod` 3 == 0 = 'O'
      | n `mod` 5 == 0 = 'm'
      | otherwise = 'M'

grid :: [(Int, Int)] -> [[Maybe Int]]
grid coords = trimEdges [[fill (x, y) | x <- [0 .. 399]] | y <- [-0 .. 399]]
  where
    fill (x, y)
      | (x, y) `elem` coords = Nothing
      | otherwise = closestUnique (x, y)
    coordsAndIds = zip coords [0 ..]
    closestUnique :: (Int, Int) -> Maybe Int
    closestUnique (x', y') =
      case argmins (\(c, i) -> (distance (x', y') c, i)) coordsAndIds of
        [(_, i)] -> Just i
        _        -> Nothing

grid2 :: [(Int, Int)] -> [[Maybe Int]]
grid2 coords = [[fill (x, y) | x <- [-500 .. 1000]] | y <- [-500 .. 1000]]
  where
    fill (x, y)
      | withinRegion2 coords (x, y) = Just 3
      | otherwise = Nothing

withinRegion2 :: [(Int, Int)] -> (Int, Int) -> Bool
withinRegion2 coords coord = (< 10000) (sum $ map (distance coord) coords)

trimEdges gd = map (map nothingIfTouchingEdge) gd
  where
    nothingIfTouchingEdge :: Maybe Int -> Maybe Int
    nothingIfTouchingEdge Nothing = Nothing
    nothingIfTouchingEdge x
      | x `Set.member` touchers = Nothing
      | otherwise = x
    touchers :: Set (Maybe Int)
    touchers = Set.fromList $ borderElems gd

borderElems :: [[a]] -> [a]
borderElems [] = []
borderElems xs = concat [first, (concatMap firstAndLast rest), last]
  where
    (first, rest, last) = firstRestAndLast xs

firstAndLast :: [a] -> [a]
firstAndLast (first:rest'') = [first, last]
  where
    (last:rest') = reverse rest''

firstRestAndLast :: [a] -> (a, [a], a)
firstRestAndLast (first:rest'') = (first, rest, last)
  where
    (last:rest') = reverse rest''
    rest = reverse rest'

last :: [a] -> a
last = head . reverse

parse :: String -> Maybe (Int, Int)
parse = parseMaybe parser
  where
    parser = do
      x <- digits
      char ','
      char ' '
      y <- digits
      return (x, y)
    digits :: ReadP Int
    digits = read <$> munch1 isDigit

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
