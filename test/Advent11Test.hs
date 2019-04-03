module Advent11Test
  ( test_11
  ) where

import           Advent11
import           Control.Monad (forM_)
import           Debug.Trace
import           Test.Hspec
import           Text.Printf

testCells =
  [ ((3, 5), 8, 4)
  , ((122, 79), 57, -5)
  , ((217, 196), 39, 0)
  , ((101, 153), 71, 4)
  ]

testTable = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]]

testTableResult =
  [[1, 3, 6, 10], [6, 14, 24, 36], [15, 33, 54, 78], [28, 60, 96, 136]]

test_11 :: IO ()
test_11 = do
  hspec $ do
    describe "summedAreaTable" $
      it "creates a summed area table for a known matrix" $
      summedAreaTable 4 testTable == testTableResult
    describe "sumForArea" $ do
      it "sums an area at the top and left edges" $
        sumForArea testTableResult (3, 3) (1, 1) == 54
      it "sums an area at the top and right edges" $
        sumForArea testTableResult (3, 3) (2, 1) == 63
      it "sums an area at the bottom and left edges" $
        sumForArea testTableResult (3, 3) (1, 2) == 90
      it "sums an area at the bottom and right edges" $
        sumForArea testTableResult (3, 3) (2, 2) == 99
    describe "scoreCell" $
      forM_ testCells $ \((x, y), serial, expected) ->
        it
          (printf
             "returns `%d` for cell %d, %d with serial %d"
             expected
             x
             y
             serial) $
        scoreCell serial (x, y) `shouldBe` expected
