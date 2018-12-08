module Advent06Test
  ( test_06
  ) where

import           Advent06
import           Control.Monad (forM_)
import           Test.Hspec
import           Text.Printf

tests = [("a", "a")]

test_06 :: IO ()
test_06 =
  hspec $ do
    describe "something" $
      forM_ tests $ \(input, expected) ->
        it (printf "should produce `%s` for input `%s`" expected input) $
        id input `shouldBe` expected
