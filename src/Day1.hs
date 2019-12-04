module Day1 (solve) where

import Text.Trifecta


parseModule :: Parser Int
parseModule = (fromIntegral <$> integer)

mass2fuel :: Int -> Int
mass2fuel mass = mass `div` 3 - 2

mass2fuel2 :: Int -> Int
mass2fuel2 = sum . takeWhile (> 0) . tail . iterate mass2fuel

solve :: String -> IO ()
solve filepath = do
  Just xs <- parseFromFile (manyTill parseModule eof) filepath
  print . sum $ map mass2fuel xs
  print . sum $ map mass2fuel2 xs
