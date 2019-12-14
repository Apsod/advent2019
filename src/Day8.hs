module Day8 (solve) where

import Text.Trifecta
import Data.Char
import Control.Monad
import Data.Bool
import Data.Monoid
import Data.List
import Data.Ord
import Control.Applicative

parseLayer :: Int -> Int -> Parser [Color]
parseLayer width height = 
  let parseRow = count width (cmap . digitToInt <$> digit)
  in join <$> count height parseRow

parseData :: Int -> Int -> Parser [[Color]]
parseData width height = many (parseLayer width height)

countPred :: (a -> Bool) -> [a] -> Int
countPred pred = getSum . foldMap (bool (Sum 0) (Sum 1) . pred)

data Color = Black | White | Transparent
  deriving(Eq, Ord, Show)

instance Semigroup Color where
  Black <> _ = Black
  White <> _ = White
  Transparent <> x = x

instance Monoid Color where
  mempty = Transparent


color2ascii :: Color -> Char
color2ascii Black = '#'
color2ascii White = ' '
color2ascii Transparent = undefined


cmap :: Int -> Color
cmap 0 = Black
cmap 1 = White
cmap 2 = Transparent

cinv :: Color -> Int
cinv Black = 0
cinv White = 1
cinv Transparent = 2

pp :: Int -> [Color] -> IO ()
pp width xs = go xs
  where 
    go [] = return ()
    go xs = do 
      let (h, xs') = splitAt width xs
      putStrLn (fmap color2ascii h)
      go xs'

solve :: String -> IO ()
solve filepath = do
  Just layers <- parseFromFile (parseData 25 6) filepath
  let l = minimumBy (comparing (countPred (==Black))) layers
  print $ (countPred (==White) l) * (countPred (==Transparent) l)
  pp 25 . getZipList . foldl1 (\a b -> (<>) <$> a <*> b) $ fmap ZipList layers

