module Advent03
  ( exec_03
  , Claim(..)
  , parseClaim
  , claimCoordinates
  , claimsPerCoordinate
  ) where

import           Data.Char
import qualified Data.Map.Strict              as Map
import           Data.Maybe
import           Text.ParserCombinators.ReadP

data Claim =
  Claim Int
        (Int, Int)
        (Int, Int)
  deriving (Show, Eq)

exec_03 :: IO ()
exec_03 = do
  putStrLn "*********"
  putStrLn "* Day 3 *"
  putStrLn "*********"
  claims <-
    fmap (map (fromJust . parseClaim) . lines) $ readFile "resources/03.txt"
  let claimMap = claimsPerCoordinate claims
  putStrLn $
    "Overlapping Claim Inches: " ++
    show (length $ filter (> 1) $ Map.elems $ claimMap)
  let nonOverlappingClaims = filter (noOverlaps claimMap) $ claims
  let Claim nonOverlappingClaimId _ _ = head $ nonOverlappingClaims
  putStrLn $ "Non-overlapping claims: " ++ show nonOverlappingClaims
  putStrLn $ "Non-overlapping claim: " ++ show nonOverlappingClaimId

type ClaimMap = Map.Map (Int, Int) Int

claimsPerCoordinate :: [Claim] -> ClaimMap
claimsPerCoordinate claims = claimMap
  where
    claimMap = foldl count Map.empty (concatMap claimCoordinates claims)
    count m coords = Map.insertWith (+) coords 1 m

noOverlaps :: ClaimMap -> Claim -> Bool
noOverlaps claimMap claim =
  all (\coords -> fmap (< 2) (Map.lookup coords claimMap) == Just True) $
  claimCoordinates claim

claimCoordinates :: Claim -> [(Int, Int)]
claimCoordinates (Claim _ (x, y) (w, h)) =
  [(x', y') | x' <- [x .. x + w - 1], y' <- [y .. y + h - 1]]

parseClaim :: String -> Maybe Claim
parseClaim str =
  case parsed of
    Nothing                   -> Nothing
    Just (id, (x, y), (w, h)) -> Just $ Claim id (x, y) (w, h)
  where
    parsed = parseMaybe parser str
    parser :: ReadP (Int, (Int, Int), (Int, Int))
    parser = do
      char '#'
      id <- digits
      skipSpaces
      char '@'
      skipSpaces
      x <- digits
      char ','
      y <- digits
      char ':'
      skipSpaces
      w <- digits
      char 'x'
      h <- digits
      return (id, (x, y), (w, h))
    digits :: ReadP Int
    digits = fmap read $ munch1 isDigit

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
  case readP_to_S parser input of
    []              -> Nothing
    ((result, _):_) -> Just result
