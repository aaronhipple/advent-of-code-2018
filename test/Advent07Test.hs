module Advent07Test
  ( test_07
  ) where

import           Advent07
import           Data.List   (sort, tails)
import           Data.Set    (Set (..))
import qualified Data.Set    as Set
import           Test.Hspec
import           Text.Printf

instructions =
  [ "Step C must be finished before step A can begin."
  , "Step C must be finished before step F can begin."
  , "Step A must be finished before step B can begin."
  , "Step A must be finished before step D can begin."
  , "Step B must be finished before step E can begin."
  , "Step D must be finished before step E can begin."
  , "Step F must be finished before step E can begin."
  ]

expectedOrder = "CABDFE"

realGraph =
  Graph
    [ ('G', 'W')
    , ('X', 'S')
    , ('F', 'V')
    , ('C', 'Y')
    , ('M', 'J')
    , ('K', 'Z')
    , ('U', 'W')
    , ('I', 'H')
    , ('W', 'B')
    , ('A', 'Y')
    , ('Y', 'D')
    , ('S', 'Q')
    , ('N', 'V')
    , ('H', 'D')
    , ('D', 'Q')
    , ('L', 'E')
    , ('Q', 'E')
    , ('T', 'R')
    , ('J', 'P')
    , ('R', 'E')
    , ('E', 'V')
    , ('O', 'P')
    , ('P', 'B')
    , ('Z', 'V')
    , ('B', 'V')
    , ('Y', 'B')
    , ('C', 'B')
    , ('Q', 'T')
    , ('W', 'P')
    , ('X', 'Z')
    , ('L', 'T')
    , ('G', 'Y')
    , ('Y', 'R')
    , ('E', 'B')
    , ('X', 'E')
    , ('Y', 'V')
    , ('H', 'L')
    , ('L', 'J')
    , ('S', 'T')
    , ('F', 'T')
    , ('Y', 'J')
    , ('A', 'H')
    , ('P', 'Z')
    , ('R', 'O')
    , ('X', 'F')
    , ('I', 'O')
    , ('Y', 'Q')
    , ('S', 'D')
    , ('Q', 'B')
    , ('C', 'D')
    , ('Y', 'N')
    , ('O', 'Z')
    , ('G', 'D')
    , ('A', 'O')
    , ('U', 'N')
    , ('Y', 'P')
    , ('E', 'O')
    , ('I', 'Q')
    , ('W', 'O')
    , ('D', 'B')
    , ('Z', 'B')
    , ('L', 'B')
    , ('P', 'V')
    , ('C', 'E')
    , ('S', 'O')
    , ('U', 'T')
    , ('U', 'O')
    , ('Y', 'L')
    , ('N', 'L')
    , ('Q', 'Z')
    , ('U', 'L')
    , ('U', 'D')
    , ('J', 'O')
    , ('L', 'R')
    , ('S', 'P')
    , ('H', 'R')
    , ('F', 'I')
    , ('D', 'T')
    , ('C', 'M')
    , ('W', 'D')
    , ('R', 'V')
    , ('U', 'S')
    , ('K', 'R')
    , ('D', 'V')
    , ('D', 'R')
    , ('I', 'E')
    , ('L', 'O')
    , ('T', 'Z')
    , ('A', 'E')
    , ('D', 'Z')
    , ('H', 'V')
    , ('A', 'L')
    , ('W', 'R')
    , ('F', 'A')
    , ('Y', 'Z')
    , ('I', 'P')
    , ('F', 'J')
    , ('H', 'B')
    , ('G', 'Z')
    , ('C', 'K')
    , ('D', 'E')
    ]

test_07 :: IO ()
test_07 =
  hspec $ do
    describe "makeGraph" $
      it "should produce a graph for given instructions" $
      makeGraph instructions `shouldBe`
      Graph
        [ ('C', 'A')
        , ('C', 'F')
        , ('A', 'B')
        , ('A', 'D')
        , ('B', 'E')
        , ('D', 'E')
        , ('F', 'E')
        ]
    describe "findFirsts" $
      it "should find the first node(s) in a graph" $
      Set.toList (findFirsts $ makeGraph instructions) `shouldBe` "C"
    describe "findLasts" $
      it "should find the last node(s) in a graph" $
      Set.toList (findLasts $ makeGraph instructions) `shouldBe` "E"
    describe "sortInstructions" $
      it (printf "should produce expected `%s` for known input" expectedOrder) $
      sortInstructions instructions `shouldBe` expectedOrder
    describe "runTicks" $ do
      it "should produce expected `15` for known input" $
        (snd $ snd $ runTicks 2 0 (makeGraph instructions)) `shouldBe` 15
      it "should produce expected `258` for known input" $ -- found a hint
        (snd $ snd $ runTicks 2 60 (makeGraph instructions)) `shouldBe` 258
    describe "realGraph" $ do
      describe "trav" $ do
        it "visits all nodes" $
          Set.fromList (trav realGraph) `shouldBe` allNodes realGraph
        it "visits each node only once" $
          Set.toList (Set.fromList (trav realGraph)) `shouldBe`
          (sort $ trav realGraph)
        it "visits each node only when its dependencies are met" $
          all
            (\vs' ->
               case vs' of
                 []     -> True
                 (v:vs) -> allMet realGraph (Set.fromList vs) v)
            (tails $ reverse $ trav realGraph) `shouldBe`
          True
      describe "runticks" $ do
        it "visits all nodes" $
          Set.fromList (fst $ snd $ runTicks 5 60 realGraph) `shouldBe`
          allNodes realGraph
        it "visits each node only once" $
          Set.toList (Set.fromList (fst $ snd $ runTicks 5 60 realGraph)) `shouldBe`
          (sort $ (fst $ snd $ runTicks 5 60 realGraph))
        it "visits each node only when its dependencies are met" $
          all
            (\vs' ->
               case vs' of
                 []     -> True
                 (v:vs) -> allMet realGraph (Set.fromList vs) v)
            (tails $ reverse $ (fst $ snd $ runTicks 5 60 realGraph)) `shouldBe`
          True
        it "should produce the same output as `trav` for 1 worker and 0 delay" $
          (fst $ snd $ runTicks 1 0 realGraph) `shouldBe` trav realGraph
