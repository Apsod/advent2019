module Day7(solve) where

import Text.Trifecta
import Intcode
import Data.List
import Control.Monad
import Control.Monad.ST
import Data.Maybe

fromSingleton :: [a] -> a
fromSingleton [x] = x

findConf :: Program -> String
findConf program =
  let
    f signal phase = fromSingleton . snd $ runProgram program [phase, signal]
    output phases = foldl f 0 phases
  in show $ maximum (output <$> permutations [0..4])


findConf_2 :: Program -> String
findConf_2 program = 
  let 
    run_permutation phases = runST $ do
      states@[s0,s1,s2,s3,s4] <- replicateM 5 (initialize program)
      mapM_ (\(phase, state) -> putInput state phase) (zip phases states)
      putInput s0 0
      let 
        go i = do
          let 
            s = states !! i
            i' = (i+1) `mod` 5
            s' = states !! i'
          (f, o) <- run s
          mapM_ (putInput s') o
          if f && i==4 then
            return $ last o
          else
            go i'
      go 0
  in show $ maximum (run_permutation <$> permutations [5..9])


solve :: String -> IO ()
solve filepath = do 
  Just xs <- parseFromFile parseProgram filepath
  putStrLn $ findConf xs
  putStrLn $ findConf_2 xs
