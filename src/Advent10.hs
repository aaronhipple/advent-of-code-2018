module Advent10
  ( exec_10
  , parsePoints
  , Field(..)
  , step
  ) where

import           Data.Char
import           Data.Maybe                   (fromJust)
import           Data.Set                     (Set (..))
import qualified Data.Set                     as S
import           Text.ParserCombinators.ReadP
import           Utils                        (parseMaybe)

exec_10 :: IO ()
exec_10 = do
  putStrLn "**********"
  putStrLn "* Day 10 *"
  putStrLn "**********"
  field <- parsePoints <$> readFile "resources/10.txt"
  print $ crop (iterate step field !! 10727)

data Field =
  Field ((Int, Int), (Int, Int))
        [((Int, Int), (Int, Int))]

step :: Field -> Field
step (Field bnds pts) = Field bnds pts'
  where
    pts' = map movePoint pts
    movePoint ((x, y), (dx, dy)) = ((x + dx, y + dy), (dx, dy))

instance Show Field where
  show (Field ((wMin, wMax), (hMin, hMax)) pts) =
    unlines
      [ [ if (x, y) `S.member` p
        then '#'
        else '.'
      | x <- [wMin .. wMax]
      ]
      | y <- [hMin .. hMax]
      ]
    where
      p = S.fromList $ map fst pts

area :: Field -> Int
area (Field _ pts) = (wMax - wMin) * (hMax - hMin)
  where
    ((wMin, wMax), (hMin, hMax)) = bounds (map fst pts)

crop :: Field -> Field
crop (Field _ pts) = Field ((wMin, wMax), (hMin, hMax)) pts
  where
    ((wMin, wMax), (hMin, hMax)) = bounds $ map fst pts

bounds :: [(Int, Int)] -> ((Int, Int), (Int, Int))
bounds pts = ((wMin, wMax), (hMin, hMax))
  where
    wMin = minimum $ map fst pts
    wMax = maximum $ map fst pts
    hMin = minimum $ map snd pts
    hMax = maximum $ map snd pts

parsePoints :: String -> Field
parsePoints s = Field (bounds (map fst $ pts)) pts
  where
    pts = map (fromJust . parsePoint) (lines s)

parsePoint :: String -> Maybe ((Int, Int), (Int, Int))
parsePoint = parseMaybe parser
  where
    parser = do
      (x, y) <- pair "position"
      (dx, dy) <- pair "velocity"
      return $ ((x, y), (dx, dy))
    pair label = do
      skipSpaces
      string $ label ++ "=<"
      a <- int
      char ','
      b <- int
      char '>'
      skipSpaces
      return (a, b)
    int :: ReadP Int
    int = do
      skipSpaces
      n <- choice [negative, digits]
      skipSpaces
      return n
      where
        negative = do
          char '-'
          x <- digits
          return $ negate x
        digits = do
          x <- munch1 isDigit
          return $ read x
