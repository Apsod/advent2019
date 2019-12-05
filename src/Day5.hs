{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, FlexibleContexts #-}

module Day5 (solve) where

import Text.Trifecta
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST
import Data.Bool

type Program = UArray Int Int

list2program :: [Int] -> Program
list2program xs = listArray (0, length xs - 1) xs

int :: Parser Int
int = fromIntegral <$> integer

parseProgram :: Parser Program
parseProgram = list2program <$> (int `sepBy` comma)

type Mode = Int

parseOp :: Int -> (Int, Mode, Mode, Mode)
parseOp x = (opcode, mode1, mode2, mode3)
  where
    [opcode, mode1, mode2, mode3] = map snd . take 4 $ iterate (\(x,_) -> x `divMod` 10) (x `divMod` 100)


runProgram :: [Int] -> Program -> (Int, [Int])
runProgram input  program = runST $ do
  state <- thaw program :: forall s. ST s (STUArray s Int Int)
  let read 0 ix = readArray state ix >>= readArray state
      read 1 ix = readArray state ix
      write 0 ix val = do
        p <- readArray state ix
        writeArray state p val
      go input output ix = do
        (op, m1, m2, m3) <- parseOp <$> read 1 ix
        case op of
          1 -> do
            ((+) <$> read m1 (ix + 1) <*> read m2 (ix + 2)) >>= write m3 (ix+3)
            go input output (ix + 4)
          2 -> do
            ((*) <$> read m1 (ix + 1) <*> read m2 (ix + 2)) >>= write m3 (ix+3)
            go input output (ix + 4)
          3 -> do
            write m1 (ix+1) (head input)
            go (tail input) output (ix + 2)
          4 -> do
            p <- read m1 (ix+1)
            go input (p:output) (ix + 2)
          5 -> do
            p <- (/=0) <$> read m1 (ix+1)
            case p of
              True -> read m2 (ix+2) >>= go input output
              False -> go input output (ix+3)
          6 -> do
            p <- (==0) <$> read m1 (ix+1)
            case p  of
              True -> read m2 (ix+2) >>= go input output
              False -> go input output (ix+3)
          7 -> do
            p <- (<) <$> read m1 (ix+1) <*> read m2 (ix+2)
            write m3 (ix+3) (bool 0 1 p)
            go input output (ix+4)
          8 -> do
            p <- (==) <$> read m1 (ix+1) <*> read m2 (ix+2)
            write m3 (ix+3) (bool 0 1 p)
            go input output (ix+4)
          99 -> do
            diagnostic <- read 1 0
            return (diagnostic, reverse output)
  go input [] 0


solve :: String -> IO ()
solve filepath = do
  Just xs <- parseFromFile parseProgram filepath
  print $ runProgram [1] xs
  print $ runProgram [5] xs
