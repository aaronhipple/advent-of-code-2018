module Advent03Test
  ( test_03
  ) where

import           Advent03      (Claim (..), parseClaim)
import           Control.Monad (forM_)
import           Test.Hspec
import           Text.Printf

test_03 :: IO ()
test_03 =
  hspec $ do
    describe "true" $ it "should be true" $ True `shouldBe` True
    describe "parseClaim" $
      forM_ testCases $ \(input, expected) ->
        it (printf "should correctly parse claim `%s`" input) $
        parseClaim input `shouldBe` expected

testCases =
  [ ("#1 @ 1,3: 4x4", Just (Claim 1 (1, 3) (4, 4)))
  , ("#2 @ 3,1: 4x4", Just (Claim 2 (3, 1) (4, 4)))
  , ("#3 @ 5,5: 2x2", Just (Claim 3 (5, 5) (2, 2)))
  , ("INVALID CLAIM", Nothing)
  , ("#532000 @ 52,51: 20x200", Just (Claim 532000 (52, 51) (20, 200)))
  , ("#123@5,10:12x345", Just (Claim 123 (5, 10) (12, 345)))
  ]
