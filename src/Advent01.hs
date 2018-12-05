module Advent01
  ( exec_01
  , progressiveSums
  , findRepeat
  ) where

import           Data.Maybe (fromJust)
import           Data.Set   (empty, insert, member)

exec_01 :: IO ()
exec_01 = do
  deltas <- fmap parse (readFile "resources/01.txt")
  putStrLn $ "Sum: " ++ show (sum deltas)
  putStrLn $
    "Repeat: " ++ show (fromJust $ findRepeat (progressiveSums $ cycle deltas))

parse :: String -> [Integer]
parse = map toNumber . lines
  where
    toNumber :: String -> Integer
    toNumber ('+':rest) = read rest
    toNumber ('-':rest) = -1 * read rest

progressiveSums :: [Integer] -> [Integer]
progressiveSums = f 0
  where
    f _ [] = []
    f carry (x:xs) = next : f next xs
      where
        next = carry + x

findRepeat :: [Integer] -> Maybe Integer
findRepeat = f empty
  where
    f seen [] = Nothing
    f seen (n:ns)
      | n `member` seen = Just n
      | otherwise = f (insert n seen) ns
