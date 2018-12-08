module Advent04
  ( exec_04
  , Entry(..)
  , Action(..)
  , parseEntry
  ) where

import           Control.Applicative          ((<$>))
import           Data.Char
import           Data.Foldable                (maximumBy)
import           Data.List                    (sortOn)
import           Data.List.NonEmpty           (fromList)
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (fromJust, fromMaybe)
import qualified Data.Set                     as Set
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import           Text.ParserCombinators.ReadP
import           Utils                        (countMap, keyWithMaxValue,
                                               mostCommon, mostCommonWithCount,
                                               parseMaybe)

exec_04 :: IO ()
exec_04 = do
  putStrLn "*********"
  putStrLn "* Day 4 *"
  putStrLn "*********"
  entries <- loadEntries
  let gid = findSleepiestGuard entries
  let (minute, _) = fromMaybe (-1, -1) $ findSleepiestMinute entries gid
  putStrLn $ "Sleepiest Guard (MethodOne): " ++ show gid
  putStrLn $ "Sleepiest Guard's Sleepiest Minute (MethodOne): " ++ show minute
  putStrLn $ "Product of above: " ++ show (gid * minute)
  let (gid2, minute2) = findSleepiestGuardAndMinute entries
  putStrLn $ "Sleepiest Guard (Method2): " ++ show gid2
  putStrLn $ "Sleepiest Guard's Sleepiest Minute (Method2): " ++ show minute2
  putStrLn $ "Product of above: " ++ show (gid2 * minute2)

findSleepiestGuardAndMinute :: [Entry] -> (Int, Int)
findSleepiestGuardAndMinute entries = (gid, minute)
  where
    (gid, (minute, _)) =
      maximumBy (\(_, (_, a)) (_, (_, b)) -> compare a b) $
      Map.toList $ Map.mapMaybe id guardMap
    guardMap = Map.fromSet (findSleepiestMinute entries) guards
    guards = foldl getGuard Set.empty entries
    getGuard gs (Entry _ (StartShift gid)) = Set.insert gid gs
    getGuard gs _                          = gs

findSleepiestMinute :: [Entry] -> Int -> Maybe (Int, Integer)
findSleepiestMinute entries gid
  | null spans = Nothing
  | otherwise =
    Just $ mostCommonWithCount $ fromList $ concatMap toMinutes spans
  where
    spans = makeSpans gid entries

makeSpans :: Int -> [Entry] -> [(UTCTime, UTCTime)]
makeSpans gid = fromMaybe [] . Map.lookup gid . makeSpanMap

makeSpanMap :: [Entry] -> Map.Map Int [(UTCTime, UTCTime)]
makeSpanMap entries = spanmap
  where
    (spanmap, _, _) = foldl foldEntries (Map.empty, Nothing, Nothing) entries
    foldEntries (ms, _, _) (Entry time (StartShift gid)) =
      (ms, Just gid, Just time)
    foldEntries (ms, gid, _) (Entry time FallAsleep) = (ms, gid, Just time)
    foldEntries (ms, Just gid, Just lastTime) (Entry time WakeUp) =
      (Map.insertWith (++) gid [m] ms, Just gid, Just time)
      where
        m = (lastTime, time)

toMinutes :: (UTCTime, UTCTime) -> [Int]
toMinutes (start, end) = minutes
  where
    minutes
      | s < midnight && e < midnight =
        [1440 - (60 * todHour s + todMin s) .. 1440 -
                                               (60 * todHour e + todMin e)]
      | s < midnight && e > midnight =
        [1440 - (60 * todHour s + todMin s) .. (60 * todHour e + todMin e)]
      | s >= midnight && e >= midnight =
        [(60 * todHour s + todMin s) .. (60 * todHour e + todMin e)]
    (s, e) =
      (timeToTimeOfDay (utctDayTime start), timeToTimeOfDay (utctDayTime end))

findSleepiestGuard :: [Entry] -> Int
findSleepiestGuard entries = gid
  where
    gid = keyWithMaxValue guards
    (guards, _, _) = foldl foldEntries (Map.empty, Nothing, Nothing) entries
    foldEntries (gs, _, _) (Entry time (StartShift gid)) =
      (gs, Just gid, Just time)
    foldEntries (gs, gid, _) (Entry time FallAsleep) = (gs, gid, Just time)
    foldEntries (gs, Just gid, Just lastTime) (Entry time WakeUp) =
      ( Map.insertWith (+) gid (time `diffUTCTime` lastTime) gs
      , Just gid
      , Just time)

loadEntries :: IO [Entry]
loadEntries = do
  entries <- map (fromJust . parseEntry) . lines <$> readFile "resources/04.txt"
  return $ sortOn (\(Entry time _) -> time) entries

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
      date <-
        between
          (char '[')
          (char ']')
          (parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M" <$>
           many get)
      skipSpaces
      action <- fmap toAction (munch1 true)
      return (date, action)
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
