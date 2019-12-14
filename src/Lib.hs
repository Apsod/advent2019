module Lib
    ( someFunc
    ) where

import System.Environment
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9

solvers = [
  undefined,
  Day1.solve,
  Day2.solve,
  Day3.solve,
  Day4.solve,
  Day5.solve,
  Day6.solve,
  Day7.solve,
  Day8.solve,
  Day9.solve
  ]

someFunc :: IO ()
someFunc = do 
  [day, filepath] <- getArgs
  solvers !! (read day) $ filepath
