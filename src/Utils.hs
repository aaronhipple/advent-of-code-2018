module Utils
  ( parseMaybe
  , mostCommon
  , mostCommonWithCount
  , countMap
  , keyWithMaxValue
  , maxValue
  , argmax
  , argmaxs
  , argmin
  , argmins
  ) where

import           Data.Functor
import           Data.List.NonEmpty           (NonEmpty)
import           Data.List.NonEmpty           as NonEmpty
import qualified Data.Map.Strict              as Map
import           Data.Maybe
import           Text.ParserCombinators.ReadP (ReadP, readP_to_S)

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
  case readP_to_S parser input of
    []              -> Nothing
    ((result, _):_) -> Just result

mostCommon :: Ord a => NonEmpty a -> a
mostCommon = fst . mostCommonWithCount

mostCommonWithCount :: Ord a => NonEmpty a -> (a, Integer)
mostCommonWithCount = maxValue . countMap

countMap :: (Ord k, Num a) => NonEmpty k -> Map.Map k a
countMap = foldl f Map.empty
  where
    f mp mn = Map.insertWith (+) mn 1 mp

keyWithMaxValue :: Ord a => Map.Map k a -> k
keyWithMaxValue = fst . maxValue

maxValue :: Ord a => Map.Map k a -> (k, a)
maxValue = fromJust . Map.foldlWithKey f Nothing
  where
    f Nothing k v = Just (k, v)
    f (Just (k', v')) k v
      | v > v' = Just (k, v)
      | v <= v' = Just (k', v')

argmax f xs = Prelude.head $ (argmaxs f xs)

argmaxs :: Ord b => (a -> b) -> [a] -> [a]
argmaxs _ [] = []
argmaxs f (x:xs) = fst $ foldl g ([x], f x) xs
  where
    g (maxs, y) x
      | y' > y = ([x], y')
      | y' == y = (x : maxs, y)
      | otherwise = (maxs, y)
      where
        y' = f x

argmin f xs = Prelude.head $ (argmins f xs)

argmins :: Ord b => (a -> b) -> [a] -> [a]
argmins _ [] = []
argmins f (x:xs) = fst $ foldl g ([x], f x) xs
  where
    g (mins, y) x
      | y' < y = ([x], y')
      | y' == y = (x : mins, y)
      | otherwise = (mins, y)
      where
        y' = f x
