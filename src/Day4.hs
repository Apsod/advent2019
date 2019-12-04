module Day4 (solve) where

import Text.Trifecta
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST
import Data.List
import Data.Int

type Password = [Int8]


normalizePassword :: Password -> Password
normalizePassword (x:xs) = x : go x xs
  where
    go x (h:t) =
      let x' = max x h
      in x' : go x' t
    go _ [] = []

next :: Password -> Password
next [x] = [x + 1]
next ps@(x:9:t) = let x' = x+1 in replicate (length ps) x'
next (x:t) = x : next t

parseDecimal :: Parser Int8
parseDecimal = read . pure <$> digit

parsePassword :: Parser Password
parsePassword = count 6 parseDecimal

parseProblem :: Parser (Password, Password)
parseProblem = do
  lb <- parsePassword
  symbolic '-'
  ub <- parsePassword
  return (normalizePassword lb, ub)

valid1 :: Password -> Bool
valid1 = any (>=2) . map length . group

valid2 :: Password -> Bool
valid2 = any (==2) . map length . group

countPossible :: (Password -> Bool) -> Password -> Password -> Int
countPossible valid ub = go 0
  where 
    go count x = case (x <= ub, valid x) of
      (True, True) -> go (count + 1) (next x)
      (True, False) -> go count (next x)
      _             -> count

solve :: String -> IO ()
solve filepath = do
  Just (lb, ub) <- parseFromFile parseProblem filepath
  print $ countPossible valid1 ub lb
  print $ countPossible valid2 ub lb