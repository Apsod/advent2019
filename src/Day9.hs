{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, FlexibleContexts #-}

module Day9 (solve) where

import Text.Trifecta
import Intcode
import Control.Monad.ST

solve :: String -> IO ()
solve filepath = do 
  Just xs <- parseFromFile parseProgram filepath
  print $ runProgram xs [1]
  print $ runProgram xs [2]
