module Main where

import Lib
import System.IO (readFile)
import Data.Time (getCurrentTime)

main :: IO ()
main = do
  time <- getCurrentTime
  putStrLn (show time)
