{-# LANGUAGE TupleSections #-}

import Data.Ratio
import Data.Array
import Data.List (foldl', maximumBy, sortBy, groupBy)
import qualified Data.Map.Strict as M

import Debug.Trace

type Asteroid = (Int, Int)

-- FUCK GEOMETRY and FUCK THESE ASTEROIDS
data Oblique = PP | NP | NN | PN
  deriving (Show, Eq, Ord)

data Slope = Defined Oblique (Ratio Int)
           | VerticalPos
           | VerticalNeg
           | HorizontalPos
           | HorizontalNeg
           | Self
           deriving (Show, Eq, Ord)

type SlopeDist = (Slope, Double)
type SlopeDistMap = Array (Int, Int) SlopeDist

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

slopeBetween :: Asteroid -> Asteroid -> SlopeDist
slopeBetween (x1, y1) (x2, y2) =
  let del_y = y2 - y1
      del_x = x2 - x1
      dist = sqrt (fromIntegral (del_y * del_y + del_x * del_x)) in
      case (compare del_x 0, compare del_y 0) of
        (EQ, EQ) -> (Self, 0.0)
        (EQ, LT) -> (VerticalNeg, dist)
        (EQ, GT) -> (VerticalPos, dist)
        (LT, EQ) -> (HorizontalNeg, dist)
        (GT, EQ) -> (HorizontalPos, dist)
        (LT, LT) -> (Defined NN (del_y % del_x), dist)
        (LT, GT) -> (Defined NP (del_y % del_x), dist)
        (GT, GT) -> (Defined PP (del_y % del_x), dist)
        (GT, LT) -> (Defined PN (del_y % del_x), dist)

slopesBetween :: [Asteroid] -> SlopeDistMap
slopesBetween asteroids =
  let n = length asteroids in
      array ((0, 0), (n - 1, n - 1)) [
        ((i, j), slopeBetween a1 a2) | (i, a1) <- enumerate asteroids,
                                       (j, a2) <- enumerate asteroids
        ]

visibleAsteroids :: Int -> SlopeDistMap -> Int
visibleAsteroids i map =
  let (_, (n, _)) = bounds map
      candidates = [map ! (i, j) | j <- [0..n], j /= i]
      nearest = foldl' see M.empty candidates
      see m (slope, dist) = M.insertWith min slope dist m in
      M.size nearest

toPolar :: Asteroid -> Asteroid -> (Double, Double)
toPolar (x0, y0) (x1, y1) =
  let delX = fromIntegral $ x1 - x0
      delY = fromIntegral $ y1 - y0
      adjRadians = atan2 delY delX + (pi / 2)
      normRadians rads
        | rads < 0 = normRadians (2 * pi + rads)
        | otherwise = rads
      in (normRadians adjRadians, sqrt $ delX * delX + delY * delY)

sortPolarByDepth :: [(Asteroid, (Double, Double))]
                 -> [(Asteroid, (Double, Double))]
sortPolarByDepth = concatGroups
                 . groupBy grpPolar
                 . sortBy (\(_, p1) (_, p2) -> cmpPolar p1 p2) where
  grpPolar (_, (r1, _)) (_, (r2, _)) = abs (r1 - r2) < 0.001
  concatGroups = go []
  go [] [] = []
  go acc [] = concatGroups (reverse acc)
  go acc ([]:xs) = go acc xs
  go acc ((x:xs):ys) = x:(go (xs:acc) ys)

-- not quite - need to group by rads, take firsts, then seconds...
cmpPolar :: (Double, Double) -> (Double, Double) -> Ordering
cmpPolar (theta1, r1) (theta2, r2) = if abs (theta2 - theta1) < 0.001
  then compare r1 r2
  else compare theta1 theta2

parseAsteroids :: [String] -> [Asteroid]
parseAsteroids = concatMap parseRow . enumerate where
  parseRow :: (Int, String) -> [Asteroid]
  parseRow (j, row) = ((,j) . fst) <$> filter ((== '#') . snd) (enumerate row)

main :: IO ()
main = do
  asteroids <- parseAsteroids . lines <$> getContents
  let slopeMap = slopesBetween asteroids
      (_, (n, _)) = bounds slopeMap
      visible = enumerate [visibleAsteroids i slopeMap | i <- [0..n]]
      (which, count) = maximumBy (\v1 v2 -> compare (snd v1) (snd v2)) visible
      best = asteroids !! which
      others = filter (/= best) asteroids
      polar = zip others (toPolar best <$> others)
      sortedPolar = sortPolarByDepth polar
      ((x, y), _) = sortedPolar !! 199
  print count
  print best
  print (x, y)
  print $ x * 100 + y
