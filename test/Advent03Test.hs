module Advent03Test
  ( test_03
  ) where

import           Advent03
import           Test.Hspec

test_03 :: IO ()
test_03 = hspec $ do describe "true" $ it "is true" $ True `shouldBe` True
