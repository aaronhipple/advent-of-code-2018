module Advent04
  ( exec_04
  , Entry(..)
  , Action(..)
  , parseEntry
  ) where

import           Data.Char
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.Format
import           Text.ParserCombinators.ReadP
import           Utils                        (parseMaybe)

exec_04 :: IO ()
exec_04 = do
  putStrLn "*********"
  putStrLn "* Day 4 *"
  putStrLn "*********"

data Entry =
  Entry UTCTime
        Action
  deriving (Show, Eq)

data Action
  = StartShift Int
  | FallAsleep
  | WakeUp
  deriving (Show, Eq)

parseEntry :: String -> Maybe Entry
parseEntry str =
  case parsed of
    Nothing             -> Nothing
    Just (time, action) -> Just $ Entry time action
  where
    parsed = parseMaybe parser str
    parser :: ReadP (UTCTime, Action)
    parser = do
      date <- between (char '[') (char ']') (many get)
      skipSpaces
      action <- fmap toAction (munch1 true)
      return
        (parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M" date, action)
    toAction :: String -> Action
    toAction "wakes up" = WakeUp
    toAction "falls asleep" = FallAsleep
    toAction str = StartShift parsed
      where
        parsed = fromJust $ parseMaybe parser str
        parser :: ReadP Int
        parser = do
          string "Guard #"
          id <- fmap read (munch1 isDigit)
          skipSpaces
          string "begins shift"
          return id

true _ = True
