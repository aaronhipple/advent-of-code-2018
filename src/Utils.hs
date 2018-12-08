module Utils
  ( parseMaybe
  , mostCommon
  , mostCommonWithCount
  , countMap
  , keyWithMaxValue
  , maxValue
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
