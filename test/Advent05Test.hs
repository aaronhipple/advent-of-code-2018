module Advent05Test
  ( test_05
  ) where

import           Advent05
import           Control.Monad (forM_)
import           Test.Hspec
import           Text.Printf

tests =
  [ ("dabAcCaCBAcCcaDA", "dabCBAcaDA")
  , ("aA", "")
  , ("abBA", "")
  , ("abAB", "abAB")
  , ("aabAAB", "aabAAB")
  ]

test_05 :: IO ()
test_05 =
  hspec $ do
    describe "react" $
      forM_ tests $ \(testPolymer, expectedPolymer) ->
        it
          (printf
             "should react the test polymer `%s` to `%s`"
             testPolymer
             expectedPolymer) $
        react testPolymer `shouldBe` expectedPolymer
