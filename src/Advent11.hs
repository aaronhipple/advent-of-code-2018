module Advent11
  ( exec_11
  , scoreCell
  , summedAreaTable
  , sumForArea
  , argmax
  ) where

import           Utils       (argmax)
import Data.List

exec_11 :: IO ()
exec_11 = do
  putStrLn "**********"
  putStrLn "* Day 11 *"
  putStrLn "**********"
  putStrLn
    ("Max 3x3: " ++
     show (argmax (sumFor (3, 3)) $ gridOrigins (3, 3) 300))
  putStrLn $
    "Max NxN: " ++
    (show $
     argmax (uncurry $ sumForArea st) $
     [((n', n'), (x, y)) | n' <- [1 .. 300], (x, y) <- gridOrigins (n', n') n])

serial :: Int
serial = 8561

n = 300

t :: [[Int]]
t = table n

st :: [[Int]]
st = summedAreaTable n t

sumFor = sumForArea st 

table :: Int -> [[Int]]
table n = tab
  where tab :: [[Int]]
        !tab = [[scoreCell serial (x, y) | x <- [1 .. n]] | y <- [1 .. n]]

sumForArea :: [[Int]] -> (Int, Int) -> (Int, Int) -> Int
sumForArea !t (w', h') (x', y') = d - b - c + a
  where
    x = x' - 1
    y = y' - 1
    w = w' - 1
    h = h' - 1
    a =
      if x == 0 || y == 0
        then 0
        else t !! (y - 1) !! (x - 1)
    b =
      if y == 0
        then 0
        else t !! (y - 1) !! (x + w)
    c =
      if x == 0
        then 0
        else t !! (y + h) !! (x - 1)
    d = t !! (y + h) !! (x + w)

summedAreaTable :: Int -> [[Int]] -> [[Int]]
summedAreaTable n xss = tab
  where tab :: [[Int]]
        !tab = [[sum_ $ map (sum_ . take x) $ take y xss | x <- [1 .. n]] | y <- [1 .. n]]

scoreCell :: Int -> (Int, Int) -> Int
scoreCell serial (x, y) = digit - 5
  where
    rackId = (x + 10)
    intermediate = rackId * (serial + (rackId * y))
    digit = intermediate `div` 100 `mod` 10

gridOrigins :: (Int, Int) -> Int -> [(Int, Int)]
gridOrigins (w, h) n = ors
  where ors :: [(Int, Int)]
        ors = [(x, y) | x <- [1 .. (n - (w - 1))], y <- [1 .. (n - (h - 1))]]

sum_ = foldl' (+) 0
