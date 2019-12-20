
module Day24 (solve) where

import Text.Trifecta

parseData = undefined

solve :: String -> IO ()
solve filepath = do
  Just xs <- parseFromFile parseData filepath
  print "?"
