module Day6 (solve) where

import Text.Trifecta
import Data.Tree
import Data.Map
import Data.Maybe

type Orbits = Map String [String]

tidy :: Maybe [a] -> [a]
tidy Nothing = []
tidy (Just xs) = xs

parseOrbit :: Parser (String, String)
parseOrbit = 
  let parseName = many alphaNum
  in do
    a <- parseName
    symbolic ')'
    b <- parseName
    whiteSpace
    return (a, b)


list2tree :: (Ord a) => [(a, a)] -> a -> Tree a
list2tree xs = let
  mkKeyVal (l, r) = (l, [r])
  orbitmap = fromListWith (\(new:[]) old -> new:old) $ fmap mkKeyVal xs
  map2tree = unfoldTree (\x -> (x, tidy $ orbitmap !? x))
  in map2tree


count_orbits name [] = (1, 0)
count_orbits name ss = 
  let s1 = sum (fst <$> ss)
      s2 = sum (snd <$> ss)
  in (s1 + 1, s2 + s1)


data Distance = No | You Int | San Int | Both Int
  deriving(Show)

instance Semigroup Distance where
  You x <> San y = Both (x + y)
  San x <> You y = Both (x + y)
  No <> No = No

  You x <> _ = You x
  _ <> You x = You x

  San x <> _ = San x
  _ <> San x = San x


  Both x <> _ = Both x
  _ <> Both x = Both x

instance Monoid Distance where
  mempty = No

increment (You x) = You (x+1)
increment (San x) = San (x+1)
increment x = x

count_distance "YOU" [] = You 0
count_distance "SAN" [] = San 0
count_distance _ [] = No
count_distance name ds = increment $ mconcat ds

solve :: String -> IO ()
solve filepath = do
  Just xs <- parseFromFile (manyTill parseOrbit eof) filepath
  let orbits = list2tree xs "COM"
  print . snd $ foldTree count_orbits orbits
  print $ foldTree count_distance orbits




