module Advent02
  ( exec_02
  , checksum
  , checkstring
  , differentChars
  , findStringsThatDifferByOne
  ) where

import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust)
import qualified Data.Set        as Set
import           Debug.Trace

exec_02 :: IO ()
exec_02 = do
  putStrLn "*********"
  putStrLn "* Day 2 *"
  putStrLn "*********"
  ids <- fmap lines $ readFile "resources/02.txt"
  putStrLn $ "Checksum: " ++ show (checksum ids)
  let (a, b) = fromJust (findStringsThatDifferByOne ids)
  putStrLn $ "Adjacent IDs: " ++ show (a, b)
  putStrLn $ "Same parts: " ++ show (map fst $ filter (uncurry (==)) (zip a b))

checksum :: [String] -> Integer
checksum strs = twos * threes
  where
    checks = map checkstring strs
    twos = countTrue $ map fst checks
    threes = countTrue $ map snd checks

checkstring :: String -> (Bool, Bool)
checkstring str = (2 `Set.member` counts, 3 `Set.member` counts)
  where
    counts = Set.fromList $ Map.elems (foldl count Map.empty str)
    count cs c = Map.insertWith (+) c 1 cs

countTrue :: [Bool] -> Integer
countTrue = fromIntegral . length . filter id

findStringsThatDifferByOne :: [String] -> Maybe (String, String)
findStringsThatDifferByOne [] = Nothing
findStringsThatDifferByOne (x:xs)
  | null offByOne = findStringsThatDifferByOne xs
  | otherwise = Just (x, head offByOne)
  where
    offByOne = take 1 $ filter (\y -> differentChars x y == 1) xs

differentChars :: String -> String -> Integer
differentChars xs ys = countTrue (zipWith (/=) xs ys)
