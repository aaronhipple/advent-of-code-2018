module Advent02Test
  ( test_02
  ) where

import           Advent02
import           Control.Monad (forM_)
import           Test.Hspec
import           Text.Printf

testData =
  ( [ ("abcdef", (False, False))
    , ("bababc", (True, True))
    , ("abbcde", (True, False))
    , ("abcccd", (False, True))
    , ("aabcdd", (True, False))
    , ("abcdee", (True, False))
    , ("ababab", (False, True))
    ]
  , 12)

testIDs =
  ( ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]
  , ("fghij", "fguij"))

test_02 :: IO ()
test_02 =
  hspec $ do
    describe "checksum" $
      it "produces a known-good checksum" $
      checksum (map fst (fst testData)) `shouldBe` snd testData
    describe "checkstring" $
      forM_ (fst testData) $ \(input, expected) ->
        it (printf "produces a good tuple for input `%s`" input) $
        checkstring input `shouldBe` expected
    describe "differentChars" $
      it "counts different characters in two strings" $
      differentChars "doubt" "donut" `shouldBe` 2
    describe "findStringsThatDifferByOne" $
      it "produces a known-good set of strings" $
      findStringsThatDifferByOne (fst testIDs) `shouldBe` Just (snd testIDs)
