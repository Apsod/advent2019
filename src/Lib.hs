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
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24

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
  Day9.solve,
  Day10.solve,
  Day11.solve,
  Day12.solve,
  Day13.solve,
  Day14.solve,
  Day15.solve,
  Day16.solve,
  Day17.solve,
  Day18.solve,
  Day19.solve,
  Day20.solve,
  Day21.solve,
  Day22.solve,
  Day23.solve,
  Day24.solve
  ]

someFunc :: IO ()
someFunc = do 
  [day, filepath] <- getArgs
  solvers !! (read day) $ filepath
