module Advent09Test
  ( test_09
  ) where

import           Advent09
import           Control.Monad   (forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence   as S
import           Test.Hspec
import           Text.Printf

-- (Players, High Marble, High Score)
testCases =
  [ (9, 25, 32)
  , (10, 1618, 8317)
  , (13, 7999, 146373)
  , (17, 1104, 2764)
  , (21, 6111, 54718)
  , (30, 5807, 37305)
  ]

test_09 :: IO ()
test_09 =
  hspec $ do
    describe "turn" $ do
      describe "iterates a turn of the game" $
        forM_ testCases $ \(players, turns, highScore) ->
          it
            (printf
               "produces a high score of `%d` after `%d` turns with `%d` players"
               highScore
               turns
               players) $
          foldl max 0 (scores $ atTurn turns $ iterate turn (newGame players)) `shouldBe`
          highScore
      it "produces a good game state after some known inputs" $
        atTurn 25 (iterate turn (newGame 9)) `shouldBe`
        GameState
        { marble = 25
        , marbles =
            S.fromList
              [ 0
              , 16
              , 8
              , 17
              , 4
              , 18
              , 19
              , 2
              , 24
              , 20
              , 25
              , 10
              , 21
              , 5
              , 22
              , 11
              , 1
              , 12
              , 6
              , 13
              , 3
              , 14
              , 7
              , 15
              ]
        , marbleCount = 24
        , player = 7
        , players = 9
        , scores =
            Map.fromList
              [ (1, 0)
              , (2, 0)
              , (3, 0)
              , (4, 0)
              , (5, 32)
              , (6, 0)
              , (7, 0)
              , (8, 0)
              , (9, 0)
              ]
        , index = 12
        }
