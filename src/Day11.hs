{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, FlexibleContexts #-}

module Day11 (solve) where

import Text.Trifecta
import Intcode
import qualified Data.HashTable.Class as HC
import qualified Data.HashTable.ST.Basic as H
import Data.Mutable
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.ST

type PanelState s = H.HashTable s (Int, Int) Int
type RobotState s = URef s (Int, Int)
type DirState s = URef s Int

get_one :: State s -> ST s (Maybe Int)
get_one state = go
  where 
    go = do
      result <- step state
      case result of
        Step -> go
        Output x -> return (Just x)
        Finished -> return Nothing

get_two :: State s -> ST s (Maybe (Int, Int))
get_two state = liftM2 (,) <$> get_one state <*> get_one state

dirmap :: Int -> (Int, Int)
dirmap 0 = (0, -1)
dirmap 1 = (1, 0)
dirmap 2 = (0, 1)
dirmap 3 = (-1, 0)

dirchange :: Int -> Int -> Int
dirchange 0 d = (d + 3) `mod` 4
dirchange 1 d = (d + 1) `mod` 4

pprint :: [(Int, Int)] -> IO ()
pprint ps = 
  let
    yx = fmap (\(x,y) -> (y-ymin,x-xmin)) ps
    xmax = maximum $ fmap fst ps
    xmin = minimum $ fmap fst ps
    ymax = maximum $ fmap snd ps
    ymin = minimum $ fmap snd ps
    go _ _ [] = putStrLn ""
    go r c ps@((y,x):t) =
      case (c > xmax - xmin, r > ymax - ymin) of
        (True, False) -> putStrLn "" >> go (r+1) 0 ps
        (False, False) -> do
          case (r == y, c==x) of
            (True, True) -> putStr "#" >> go r (c+1) t
            (True, False) -> putStr " " >> go r (c+1) ps
            (False, _) -> putStr " " >> go r (c+1) ps
  in go 0 0 (sort yx)



solve_2 :: [Int] -> [(Int, Int)]
solve_2 program = runST $ do
  state <- initialize program :: ST s (State s)
  panel <- H.new :: ST s (PanelState s)
  robot <- newRef (0,0) :: ST s (RobotState s)
  direction <- newRef 0 :: ST s (URef s Int)
  H.insert panel (0,0) 1
  let
    mf (p, 0) = Nothing
    mf (p, 1) = Just p
    run = do
      pos <- readRef robot
      color <- fmap (fromMaybe 0) $ H.lookup panel pos
      putInput state color
      res <- get_two state
      case res of
        Just (paint, dc) -> do
          H.insert panel pos paint
          (dx, dy) <- modifyRef direction (dirchange dc) >> fmap dirmap (readRef direction)
          modifyRef robot (\(x,y)->(dx+x,dy+y))
          run
        Nothing -> mapMaybe mf <$> HC.toList panel
  run



solve_1 :: [Int] -> Int
solve_1 program = runST $ do
  state <- initialize program :: ST s (State s)
  panel <- H.new :: ST s (PanelState s)
  robot <- newRef (0,0) :: ST s (RobotState s)
  direction <- newRef 0 :: ST s (URef s Int)
  let
    run = do
      pos <- readRef robot
      color <- fmap (fromMaybe 0) $ H.lookup panel pos
      putInput state color
      res <- get_two state
      case res of
        Just (paint, dc) -> do
          H.insert panel pos paint
          (dx, dy) <- modifyRef direction (dirchange dc) >> fmap dirmap (readRef direction)
          modifyRef robot (\(x,y)->(dx+x,dy+y))
          run
        Nothing -> length <$> HC.toList panel
  run


solve :: String -> IO ()
solve filepath = do 
  Just xs <- parseFromFile parseProgram filepath
  print $ solve_1 xs
  pprint $ solve_2 xs

