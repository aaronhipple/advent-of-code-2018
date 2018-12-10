module Advent08
  ( exec_08
  , parseLicense
  , LicenseTree(..)
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

data LicenseTree a =
  Node [LicenseTree a]
       a
  deriving (Show, Eq)

instance Foldable LicenseTree where
  foldMap f (Node cs x) = foldMap (foldMap f) cs `mappend` f x

sumAll :: LicenseTree [Int] -> Int
sumAll = sum . foldl (++) []

value :: LicenseTree [Int] -> Int
value (Node [] meta) = sum meta
value (Node cs meta) = sum $ map value sumNodes
  where
    sumNodes = mapMaybe (getChild cs) meta
    getChild xs i
      | i < 1 || i > length xs = Nothing
      | otherwise = Just $ xs !! (i - 1)

parseLicense :: String -> Maybe (LicenseTree [Int])
parseLicense = parseMaybe parseLicenseNode
  where
    parseLicenseNode = do
      skipSpaces
      childCount <- digits
      metaCount <- digits
      childNodes <- count childCount parseLicenseNode
      metaNodes <- count metaCount digits
      skipSpaces
      return $ Node childNodes metaNodes
    digits = do
      skipSpaces
      n <- fmap (read) $ munch1 isDigit
      skipSpaces
      return n
