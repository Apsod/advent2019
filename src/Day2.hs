{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, FlexibleContexts #-}

module Day2 (solve) where

import Text.Trifecta
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST

type Program = UArray Int Int

list2program :: [Int] -> Program
list2program xs = listArray (0, length xs - 1) xs

int :: Parser Int
int = fromIntegral <$> integer

parseProgram :: Parser Program
parseProgram = list2program <$> (int `sepBy` comma)

runProgram :: Int -> Int -> Program -> Int
runProgram noun verb program = runST $ do
  state <- thaw program :: forall s. ST s (STUArray s Int Int)
  let read = readArray state
      write = writeArray state
      run f ix = do
        x <- f <$>
            (read (ix + 1) >>= read) <*>
            (read (ix + 2) >>= read)
        c <- read (ix + 3)
        write c x
        go (ix + 4)
      go ix = do
        op <- read ix
        case op of
          1 -> run (+) ix
          2 -> run (*) ix
          99 -> read 0
  write 1 noun
  write 2 verb
  go 0


findAnswer :: Program -> Int -> [Int]
findAnswer program ans =  do
  noun <- [0..99]
  verb <- [0..99]
  case runProgram noun verb program == ans of
    True -> [noun*100 + verb]
    False -> []


solve :: String -> IO ()
solve filepath = do 
  Just xs <- parseFromFile parseProgram filepath
  print $ runProgram 12 2 xs
  print . head $ findAnswer xs 19690720
