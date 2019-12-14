{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, FlexibleContexts #-}

module Intcode where

import Text.Trifecta (integer, comma, sepBy, Parser)
import qualified Data.HashTable.ST.Basic as H
import Control.Monad.ST
import Control.Monad
import Data.Bool
import Data.Maybe
import Debug.Trace
import Data.List
import Data.Mutable

type Program = [Int]

type ProgramState s = H.HashTable s Int Int
type HeadState s = URef s Int
type InputState s = UDeque s Int


data State s = State {
    getHead :: HeadState s,
    getRel :: HeadState s,
    getProgram :: ProgramState s,
    getInput :: InputState s
  }

int :: Parser Int
int = fromIntegral <$> integer

parseProgram :: Parser Program
parseProgram = (int `sepBy` comma)

type Opcode = (Int, Mode, Mode, Mode)
type Modes = (Mode, Mode, Mode)
type Mode = Int

parseOp :: Int -> Opcode
parseOp x = (opcode, mode1, mode2, mode3)
  where
    [opcode, mode1, mode2, mode3] = map snd . take 4 $ iterate (\(x,_) -> x `divMod` 10) (x `divMod` 100)


data Result = Step | Waiting | Output Int | Finished


readAt :: State s -> Int -> ST s Int
readAt state ix = fromMaybe 0 <$> H.lookup (getProgram state) ix

writeAt :: State s -> Int -> Int -> ST s ()
writeAt state ix val = H.insert (getProgram state) ix val

putInput :: State s -> Int ->  ST s ()
putInput state val = pushBack (getInput state) val

moveHead :: State s -> Int -> ST s ()
moveHead state offset = modifyRef (getHead state) (\x-> x + offset)

moveRel :: State s -> Int -> ST s ()
moveRel state offset = modifyRef (getRel state) (\x-> x + offset)

stepGet :: State s -> Mode -> ST s Int
stepGet state mode = do
  ix <- readHead state
  val <- case mode of
    0 -> readAt state ix >>= readAt state
    1 -> readAt state ix
    2 -> do 
      offset <- readAt state ix
      relbase <- readRel state
      readAt state (relbase + offset)
  moveHead state 1
  return val

stepPut :: State s -> Mode -> Int -> ST s ()
stepPut state mode val = do
  ix <- readHead state
  case mode of
    0 -> readAt state ix >>= \pos -> writeAt state pos val
    1 -> writeAt state ix val
    2 -> do 
      offset <- readAt state ix
      relbase <- readRel state
      writeAt state (relbase + offset) val
  moveHead state 1

seekHead :: State s -> Int -> ST s ()
seekHead state ix = writeRef (getHead state) ix

readHead :: State s -> ST s Int
readHead state = readRef (getHead state)

readRel :: State s -> ST s Int
readRel state = readRef (getRel state)

apply :: State s -> (Int -> Int -> Int) -> Modes -> ST s Result
apply state f (m1, m2, m3) = do
  val <- f <$> stepGet state m1 <*> stepGet state m2
  stepPut state m3 val
  return Step

readInput :: State s -> Mode -> ST s Result
readInput state m1 = do
  mx <- popFront (getInput state)
  case mx of
    Just x -> stepPut state m1 x >> return Step
    Nothing -> moveHead state (-1) >> return Waiting

output :: State s -> Mode -> ST s Result
output state m1 = Output <$> stepGet state m1

jumpIf ::  State s ->  (Int -> Bool) -> Modes -> ST s Result
jumpIf state pred (m1, m2, m3) = do
  flag <- pred <$> stepGet state m1
  case flag of
    True -> stepGet state m2 >>= seekHead state >> return Step
    False -> moveHead state 1 >> return Step

writePred :: State s -> (Int -> Int -> Bool) -> Modes -> ST s Result
writePred state pred (m1, m2, m3) = do
  flag <- pred <$> stepGet state m1 <*> stepGet state m2
  stepPut state m3 (bool 0 1 flag) >> return Step

moveRelBase :: State s -> Mode -> ST s Result
moveRelBase state m1 = do
  stepGet state m1 >>= moveRel state >> return Step

step :: State s -> ST s Result
step state = do
  (op, m1, m2, m3) <- parseOp <$> stepGet state 1
  --trace (show (op, m1, m2, m3)) (return ())
  let modes = (m1, m2, m3)
  case op of
    1 -> apply state (+) modes
    2 -> apply state (*) modes
    3 -> readInput state m1
    4 -> output state m1
    5 -> jumpIf state (/=0) modes
    6 -> jumpIf state (==0) modes
    7 -> writePred state (<) modes
    8 -> writePred state (==) modes
    9 -> moveRelBase state m1
    99 -> return Finished


thaw :: [Int] -> ST s (ProgramState s)
thaw xs = do
  program <- H.newSized $ (length xs) * 2
  mapM_ (\(i,v) -> H.insert program i v) (zip [0..] xs)
  return program

initialize :: Program -> ST s (State s)
initialize program = State <$> newRef 0 <*> newRef 0 <*> thaw program <*> newColl

run :: State s -> ST s (Bool, [Int])
run state = go []
  where
    go acc = do
      result <- step state
      case result of
        Step -> go acc
        Output x -> go (x:acc)
        Finished -> return (True, reverse acc)
        Waiting -> return (False, reverse acc)

runProgram :: Program -> [Int] -> (Int, [Int])
runProgram program input = runST $ do
  state <- initialize program
  forM_ input (putInput state)
  (True, acc) <- run state
  val <- readAt state 0
  return (val, acc)
