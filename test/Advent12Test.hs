module Advent12Test
  ( test_12
  ) where

import           Advent12
import           Test.Hspec

test_12 :: IO ()
test_12 = hspec $ describe "true" $ it "is true" $ True == True
