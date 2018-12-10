module Advent08Test
  ( test_08
  ) where

import           Advent08
import           Test.Hspec

license = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

result = Node [Node [] [10, 11, 12], Node [Node [] [99]] [2]] [1, 1, 2]

test_08 :: IO ()
test_08 =
  hspec $ do
    describe "parseLicense" $
      it "should produce a tree" $ parseLicense license `shouldBe` Just result
    describe "sumAll" $
      it "should get a sum for a license tree" $
      sumAll <$> parseLicense license `shouldBe` Just 138
    describe "value" $
      it "should get a value for a license tree" $
      value <$> parseLicense license `shouldBe` Just 66
