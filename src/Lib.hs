module Lib
    ( someFunc
    ) where

import System.Environment
import Day1
import Day2
import Day3
import Day4

solvers = [undefined, Day1.solve, Day2.solve, Day3.solve, Day4.solve]

someFunc :: IO ()
someFunc = do 
  [day, filepath] <- getArgs
  solvers !! (read day) $ filepath
