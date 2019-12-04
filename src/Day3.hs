module Day3 (solve) where

import Text.Trifecta
import Data.Map
import Data.Ord
import Data.Foldable

data Delta = Delta Int Int
  deriving(Eq, Ord, Show)

instance Semigroup Delta where
  (Delta x1 y1) <> (Delta x2 y2) = Delta (x1 + x2) (y1 + y2)

instance Monoid Delta where
  mempty = Delta 0 0

manhattan :: Delta -> Int
manhattan (Delta x y) = abs(x) + abs(y)

parseDelta :: Parser [Delta]
parseDelta = f <$> oneOf "URDL" <*> (fromIntegral <$> integer)
  where
    f 'U' x = replicate x (Delta 0 1)
    f 'R' x = replicate x (Delta 1 0)
    f 'D' x = replicate x (Delta 0 (-1))
    f 'L' x = replicate x (Delta (-1) 0)

parseProblem :: Parser ([Delta], [Delta])
parseProblem =
  let parseOne = fmap concat (parseDelta `sepBy` comma)
  in (,) <$> parseOne <*> parseOne

runDelta :: [Delta] -> [Delta]
runDelta = scanl1 mappend

solve :: String -> IO ()
solve filepath = do
  Just (w1, w2) <- parseFromFile parseProblem filepath
  let
    mkPath w = fromListWith (\_ x -> x) $ zip (runDelta w) [1..]
    joint = intersectionWith (+) (mkPath w1) (mkPath w2)
  print . minimum . fmap manhattan $ keys joint
  print . minimum $ elems joint

