module Day1 (solve) where

import Text.Trifecta

newtype Module = Module {mass :: Integer}
  deriving(Show)

parseModule :: Parser Module
parseModule = (Module <$> integer)

mass2fuel :: Integer -> Integer
mass2fuel mass = mass `div` 3 - 2

mass2fuel2 :: Integer -> Integer
mass2fuel2 = sum . takeWhile (> 0) . tail . iterate mass2fuel

solve :: String -> IO ()
solve filepath = do
  Just xs <- parseFromFile (manyTill parseModule eof) filepath
  print (sum $ map (mass2fuel . mass) xs)
  print (sum $ map (mass2fuel2 . mass) xs)
