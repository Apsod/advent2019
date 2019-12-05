module Day4 (solve) where

import Text.Trifecta
import Data.List
import Data.Int

type Password = [Int8]

normalizePassword :: Password -> Password
normalizePassword = scanl1 max

parsePassword :: Parser Password
parsePassword = count 6 (read . pure <$> digit)

parseProblem :: Parser (Password, Password)
parseProblem = do
  lb <- parsePassword
  symbolic '-'
  ub <- parsePassword
  return (normalizePassword lb, ub)

next :: Password -> Password
next [x] = [x + 1]
next ps@(x:9:_) = let x' = x+1 in replicate (length ps) x'
next (x:t) = x : next t

runLength = map length . group

countPossible :: (Password -> Bool) -> Password -> Password -> Int
countPossible valid ub = length . filter valid . takeWhile (<= ub) . iterate next

solve :: String -> IO ()
solve filepath = do
  Just (lb, ub) <- parseFromFile parseProblem filepath
  print $ countPossible (any (>=2) . runLength) ub lb
  print $ countPossible (any (==2) . runLength) ub lb
