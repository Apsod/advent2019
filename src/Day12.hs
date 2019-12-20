module Day12 (solve) where

import Text.Trifecta
import qualified Data.Map as M

int :: Parser Int
int = fromIntegral <$> integer

find_cycle :: (Ord a) => [a] -> (Int, Int)
find_cycle = go M.empty . zip [0..]
  where
    go seen ((ix, val):t) = case M.lookup val seen of
      Just start -> (start, ix)
      Nothing -> go (M.insert val ix seen) t


ord2int :: Ordering -> Int
ord2int LT = 1
ord2int EQ = 0
ord2int GT = -1

step_axis :: [(Int, Int)] -> [(Int, Int)]
step_axis xs = 
  let 
    pos = fmap fst xs
    vel = fmap snd xs
    deltas = (\p -> sum $ fmap (ord2int . compare p) pos) <$> pos
    vel' = zipWith (+) vel deltas
    pos' = zipWith (+) pos vel'
  in zip pos' vel'

newtype Vec a = Vec (a, a, a)
  deriving (Eq, Show)

getX :: Vec a -> a
getX (Vec (x, _, _)) = x

getY :: Vec a -> a
getY (Vec (_, y, _)) = y

getZ :: Vec a -> a
getZ (Vec (_, _, z)) = z

instance Functor Vec where
  fmap f (Vec (a, b, c)) = Vec (f a, f b, f c)

instance Applicative Vec where
  pure x = Vec (x,x,x)
  (Vec (f, g, h)) <*> (Vec (a, b, c)) = Vec (f a, g b, h c)

data PlanetState = PlanetState {getPos :: Vec Int, getVel :: Vec Int}
  deriving (Show)

magnitude :: Vec Int -> Int
magnitude (Vec (x, y, z)) = abs x + abs y + abs z

energy :: PlanetState -> Int
energy (PlanetState pos vel) = magnitude pos * magnitude vel

parsePos :: Parser PlanetState
parsePos = between (symbolic '<') (symbolic '>') parsePos'
  where 
    parseVal = many letter >> symbolic '=' >> int
    parsePos' = do
      [x, y, z] <- commaSep parseVal
      return $ PlanetState  (Vec (x, y, z)) (pure 0)

step_one :: [PlanetState] -> PlanetState -> PlanetState
step_one planets planet = PlanetState ((+) <$> pos <*> vel') vel'
  where
    pos = getPos planet
    vel' = foldl f (getVel planet) (getPos <$> planets)
    f acc w = (+) <$> acc <*> (toD <$> pos <*> w)
    toD a b = case compare a b of
      LT -> 1
      EQ -> 0
      GT -> -1

step :: [PlanetState] -> [PlanetState]
step ps = step_one ps <$> ps

parseData :: Parser [PlanetState]
parseData = many parsePos


pprint :: [(Int, Int)] -> IO ()
pprint xs = 
  let
    ps = fmap fst xs
    vs = fmap snd xs
  in putStrLn (show ps ++ " " ++ show vs)

solve :: String -> IO ()
solve filepath = do
  Just xs <- parseFromFile parseData filepath
  print . sum . fmap energy . (!!1000) $ (iterate step xs)
  let 
    (0, xc) = find_cycle . iterate step_axis $ fmap (\(PlanetState p v) -> (getX p, getX v)) xs
    (0, yc) = find_cycle . iterate step_axis $ fmap (\(PlanetState p v) -> (getY p, getY v)) xs
    (0, zc) = find_cycle . iterate step_axis $ fmap (\(PlanetState p v) -> (getZ p, getZ v)) xs
  print (lcm xc (lcm yc zc))
