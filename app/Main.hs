module Main where

import           Control.Monad
import           Lib
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  forM_ args runDay

runDay :: String -> IO ()
runDay day =
  case day of
    "1"  -> exec_01
    "2"  -> exec_02
    "3"  -> exec_03
    "4"  -> exec_04
    "5"  -> exec_05
    "6"  -> exec_06
    "7"  -> exec_07
    "8"  -> exec_08
    "9"  -> exec_09
    "10" -> exec_10
    x    -> putStrLn $ "No such day `" ++ x ++ "`"
