module Utils
  ( parseMaybe
  ) where

import           Text.ParserCombinators.ReadP (ReadP, readP_to_S)

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
  case readP_to_S parser input of
    []              -> Nothing
    ((result, _):_) -> Just result
