{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, FlexibleContexts #-}

module Day5 (solve) where

import Text.Trifecta
import Intcode


solve :: String -> IO ()
solve filepath = do
  Just xs <- parseFromFile parseProgram filepath
  print $ runProgram xs [1]
  print $ runProgram xs [5]
