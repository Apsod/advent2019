module Day10 (solve) where

import Text.Trifecta
import Data.Bool
import Data.Ratio
import Data.Ord
import Data.List
import Data.Maybe

parseData :: Parser [Point]
parseData = go <$> many (many (oneOf ".#") <* newline)
  where 
    go rc = do 
      (y, r) <- zip [0..] rc
      (x, c) <- zip [0..] r
      bool [] [(x, y)] (c == '#')


type Point = (Integer, Integer)

data Dir = O | N | NE Rational | E | SE Rational | S | SW Rational | W | NW Rational
  deriving(Eq, Ord, Show)


dirmap :: Point -> Dir
dirmap (0, 0) = O
dirmap (x, 0) = bool W E (x > 0)
dirmap (0, y) = bool S N (y > 0)
dirmap (x, y) = case (x > 0, y > 0) of
  (True, True) -> NE (x % y)
  (True, False) -> SE (x % y)
  (False, False) -> SW (x % y)
  (False, True) -> NW (x % y)

magnitude :: Point -> Integer
magnitude (x, y) = abs x + abs y

diff :: Point -> Point -> Point
diff (x, y) (x', y') = (x'-x, y-y')

visible :: Point -> [Point] -> Int
visible p = length . group . sort . fmap (dirmap . diff p)


orderflat :: [[x]] -> [x]
orderflat [] = []
orderflat [x] = x
orderflat xs =
  let (hs, ts) = unzip $ mapMaybe uncons xs
  in hs ++ (orderflat ts)

vislist :: Point -> [Point] -> [Point]
vislist p = 
  let
    f x = let dx = diff p x in (dirmap dx, magnitude dx, x)
    order = groupBy (\(x,_,_) (y,_,_) -> x == y) . sort . fmap f
  in fmap (\(a, b, c) -> c) . orderflat . order

solve :: String -> IO ()
solve filepath = do 
  Just xs <- parseFromFile parseData filepath
  let point = maximumBy (comparing (\p -> visible p xs)) xs
      num_visible = visible point xs - 1
  print (point, num_visible)
  print ((vislist point xs) !! 200)
