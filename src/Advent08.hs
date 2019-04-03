module Advent08
  ( exec_08
  , parseLicense
  , Tree(..)
  , sumAll
  , value
  ) where

import           Data.Maybe                   (fromJust, mapMaybe)
import           Text.ParserCombinators.ReadP
import           Utils                        (parseMaybe)

import           Data.Char

exec_08 :: IO ()
exec_08 = do
  putStrLn "*********"
  putStrLn "* Day 8 *"
  putStrLn "*********"
  licenseTree <- fromJust . parseLicense <$> readFile "resources/08.txt"
  putStrLn $ "Sum: " ++ show (sumAll licenseTree)
  putStrLn $ "Value: " ++ show (value licenseTree)

data Tree a =
  Node a
       [Tree a]
  deriving (Show, Eq)

instance Foldable Tree where
  foldMap f (Node x cs) = foldMap (foldMap f) cs `mappend` f x

sumAll :: Tree [Int] -> Int
sumAll = sum . concat

value :: Tree [Int] -> Int
value (Node meta []) = sum meta
value (Node meta cs) = sum $ map value sumNodes
  where
    sumNodes = mapMaybe (getChild cs) meta
    getChild xs i
      | i < 1 || i > length xs = Nothing
      | otherwise = Just $ xs !! (i - 1)

parseLicense :: String -> Maybe (Tree [Int])
parseLicense = parseMaybe parseLicenseNode
  where
    parseLicenseNode = do
      skipSpaces
      childCount <- digits
      metaCount <- digits
      childNodes <- count childCount parseLicenseNode
      metaNodes <- count metaCount digits
      skipSpaces
      return $ Node metaNodes childNodes
    digits = do
      skipSpaces
      n <- read <$> munch1 isDigit
      skipSpaces
      return n
