module Advent04Test
  ( test_04
  ) where

import           Advent04
import           Control.Monad      (forM_)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Test.Hspec
import           Text.Printf

testEntries =
  [ ( "[1518-11-01 00:00] Guard #10 begins shift"
    , Just $
      Entry (UTCTime (ModifiedJulianDay (-124199)) (0 * 60)) (StartShift 10))
  , ( "[1518-11-01 00:05] falls asleep"
    , Just $ Entry (UTCTime (ModifiedJulianDay (-124199)) (5 * 60)) FallAsleep)
  , ( "[1518-11-01 00:25] wakes up"
    , Just $ Entry (UTCTime (ModifiedJulianDay (-124199)) (25 * 60)) WakeUp)
  , ( "[1518-11-01 00:30] falls asleep"
    , Just $ Entry (UTCTime (ModifiedJulianDay (-124199)) (30 * 60)) FallAsleep)
  , ( "[1518-11-01 00:55] wakes up"
    , Just $ Entry (UTCTime (ModifiedJulianDay (-124199)) (55 * 60)) WakeUp)
  , ( "[1518-11-01 23:58] Guard #99 begins shift"
    , Just $
      Entry (UTCTime (ModifiedJulianDay (-124199)) (1438 * 60)) (StartShift 99))
  , ( "[1518-11-02 00:40] falls asleep"
    , Just $ Entry (UTCTime (ModifiedJulianDay (-124198)) (40 * 60)) FallAsleep)
  , ( "[1518-11-02 00:50] wakes up"
    , Just $ Entry (UTCTime (ModifiedJulianDay (-124198)) (50 * 60)) WakeUp)
  , ( "[1518-11-03 00:05] Guard #10 begins shift"
    , Just $
      Entry (UTCTime (ModifiedJulianDay (-124197)) (5 * 60)) (StartShift 10))
  , ( "[1518-11-03 00:24] falls asleep"
    , Just $ Entry (UTCTime (ModifiedJulianDay (-124197)) (24 * 60)) FallAsleep)
  , ( "[1518-11-03 00:29] wakes up"
    , Just $ Entry (UTCTime (ModifiedJulianDay (-124197)) (29 * 60)) WakeUp)
  , ( "[1518-11-04 00:02] Guard #99 begins shift"
    , Just $
      Entry (UTCTime (ModifiedJulianDay (-124196)) (2 * 60)) (StartShift 99))
  , ( "[1518-11-04 00:36] falls asleep"
    , Just $ Entry (UTCTime (ModifiedJulianDay (-124196)) (36 * 60)) FallAsleep)
  , ( "[1518-11-04 00:46] wakes up"
    , Just $ Entry (UTCTime (ModifiedJulianDay (-124196)) (46 * 60)) WakeUp)
  , ( "[1518-11-05 00:03] Guard #99 begins shift"
    , Just $
      Entry (UTCTime (ModifiedJulianDay (-124195)) (3 * 60)) (StartShift 99))
  , ( "[1518-11-05 00:45] falls asleep"
    , Just $ Entry (UTCTime (ModifiedJulianDay (-124195)) (45 * 60)) FallAsleep)
  , ( "[1518-11-05 00:55] wakes up"
    , Just $ Entry (UTCTime (ModifiedJulianDay (-124195)) (55 * 60)) WakeUp)
  ]

test_04 :: IO ()
test_04 =
  hspec $ do
    describe "parseEntry" $
      forM_ testEntries $ \(input, expected) ->
        it (printf "should parse input `%s`" input) $
        parseEntry input `shouldBe` expected
