
module Day13 (solve) where

import Text.Trifecta
import qualified Intcode as I
import Data.List
import Data.Maybe
import Data.Monoid
import Debug.Trace
import qualified Data.Map as M
import Control.Monad.ST
import System.IO.Unsafe

parseData = I.parseProgram

chunk :: Int -> [a] -> [[a]]
chunk size = go
  where 
    go [] = []
    go xs = let (c, r) = splitAt size xs in c : go r

data Info = Info {getX :: Int, getY :: Int, getI :: Int}
  deriving (Eq, Show, Ord)

chunk3 :: [Int] -> [Info]
chunk3 = fmap (\[x, y, z] -> Info x y z) . chunk 3

getPos :: Info -> (Int, Int)
getPos (Info x y _) = (x, y)

pprint :: GameState -> String
pprint ps = 
  let
    yx = fmap (\((x, y), z) -> (y-ymin,x-xmin, z)) (M.assocs $ getMap ps)
    xmax = maximum . fmap fst . M.keys $ getMap ps
    xmin = minimum . fmap fst . M.keys $ getMap ps
    ymax = maximum . fmap snd . M.keys $ getMap ps
    ymin = minimum . fmap snd . M.keys $ getMap ps
    charmap 0 = ' '
    charmap 1 = 'X'
    charmap 2 = '#'
    charmap 3 = '='
    charmap 4 = 'o'
    go _ _ [] acc = acc
    go r c ps@((y,x,z):t) acc =
      case (c > xmax - xmin, r > ymax - ymin) of
        (True, False) -> go (r+1) 0 ps ('\n':acc)
        (False, False) -> 
          case (r == y, c==x) of
            (True, True) ->go r (c+1) t ((charmap z):acc)
            (True, False) ->  go r (c+1) ps (' ':acc)
            (False, _) -> go r (c+1) ps (' ':acc)
  in reverse $ go 0 0 (sort yx) ""

data GameState = GameState {
  getMap :: M.Map (Int, Int) Int,
  getBall :: Last (Int, Int),
  getPaddle :: Last (Int, Int),
  getScore :: Last Int
} 
  deriving (Show)

instance Semigroup GameState where
  (GameState m1 b1 p1 s1) <> (GameState m2 b2 p2 s2) =
    GameState (M.union m2 m1) (b1 <> b2) (p1 <> p2) (s1 <> s2)

instance Monoid GameState where
  mempty = GameState M.empty (Last Nothing) (Last Nothing) (Last Nothing)

list2last :: (Show a) => [a] -> Last a
list2last [] = Last Nothing
list2last xs = Last (Just $ last xs)

toGameState :: [Info] -> GameState
toGameState xs =
  let 
    (mscore, xs') = partition (\(Info x y i) -> (x == -1 && y == 0)) xs
    state = M.fromList $ fmap (\i -> (getPos i, getI i)) xs'
    ball = list2last . fmap getPos $ filter ((==4) . getI) xs'
    paddle = list2last . fmap getPos $ filter ((==3) . getI) xs'
    score = list2last $ fmap getI mscore
  in GameState state ball paddle score

toInfo :: (Int, Int, Int) -> Info
toInfo (x, y, i) = Info x y i

step :: I.State s -> GameState -> ST s (GameState)
step program state = do
  (finished, output) <- I.run program
  let 
    diff = toGameState $ chunk3 output
    state' = state <> diff
    (Last (Just ball)) = getBall state'
    (Last (Just paddle)) = getPaddle state'
  case compare (fst ball) (fst paddle) of
    LT -> I.putInput program (-1)
    EQ -> I.putInput program 0
    GT -> I.putInput program 1
  case finished of
    True -> return state'
    False -> step program state'

run :: I.Program -> GameState
run program = runST $ do
  state <- I.initialize program
  I.writeAt state 0 2
  step state mempty


solve :: String -> IO ()
solve filepath = do
  Just xs <- parseFromFile parseData filepath
  print . length . group . sort . filter ((== 2) . getI) . chunk3 . snd $ I.runProgram xs []
  print . getScore $ run xs
