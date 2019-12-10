{-# LANGUAGE TupleSections #-}

import Data.Ratio
import Data.Array
import Data.List (foldl')
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

parseAsteroids :: [String] -> [Asteroid]
parseAsteroids = concatMap parseRow . enumerate where
  parseRow :: (Int, String) -> [Asteroid]
  parseRow (j, row) = ((,j) . fst) <$> filter ((== '#') . snd) (enumerate row)

main :: IO ()
main = do
  asteroids <- parseAsteroids . lines <$> getContents
  let slopeMap = slopesBetween asteroids
      (_, (n, _)) = bounds slopeMap
      visible = [visibleAsteroids i slopeMap | i <- [0..n]]
      maxVisible = maximum visible
  print maxVisible
