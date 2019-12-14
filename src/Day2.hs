{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, FlexibleContexts #-}

module Day2 (solve) where

import Text.Trifecta
import Intcode
import Control.Monad.ST

runProgram_day2 :: Int -> Int -> Program -> Int
runProgram_day2 noun verb program = runST $ do
  state <- initialize program
  writeAt state 1 noun
  writeAt state 2 verb
  (True, []) <- run state
  readAt state 0

findAnswer :: Program -> Int -> [Int]
findAnswer program ans =  do
  noun <- [0..99]
  verb <- [0..99]
  case runProgram_day2 noun verb program == ans of
    True -> [noun*100 + verb]
    False -> []


solve :: String -> IO ()
solve filepath = do 
  Just xs <- parseFromFile parseProgram filepath
  print $ runProgram_day2 12 2 xs
  print . head $ findAnswer xs 19690720
