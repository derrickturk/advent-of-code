{- why "small"? it's only 1,000 pts = 1,000,000 pairs
 -   so I ain't figuring out 3d MSTs or whatnot
 -}

import Data.List (foldl', sortBy)
import qualified Data.Set as S
import Data.Ord (comparing)
import Text.Read (readMaybe)

data Point3D = Point3D Int Int Int deriving (Eq, Show, Ord)

type Circuit = S.Set Point3D

distance :: Point3D -> Point3D -> Double
distance (Point3D x1 y1 z1) (Point3D x2 y2 z2) =
  sqrt $ fromIntegral $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2

pairs :: [Point3D] -> [(Point3D, Point3D)]
pairs pts = [ (p1, p2) | p1 <- pts, p2 <- pts, p1 > p2]

parsePoint3D :: String -> Maybe Point3D
parsePoint3D line = do
  (x, ',':rest) <- pure $ break (== ',') line
  (y, ',':z) <- pure $ break (== ',') rest
  Point3D <$> readMaybe x <*> readMaybe y <*> readMaybe z

initCircuits :: [Point3D] -> [Circuit]
initCircuits = fmap S.singleton

merge :: Point3D -> Point3D -> [Circuit] -> [Circuit]
merge p1 p2 = go S.empty where
  go s [] = [s]
  go s (c:cs)
    | p1 `S.member` c = go (s `S.union` c) cs
    | p2 `S.member` c = go (s `S.union` c) cs
    | otherwise = c:go s cs

mergeAll :: [Circuit] -> [(Point3D, Point3D)] -> [Circuit]
mergeAll = foldl' (flip $ uncurry merge)

main :: IO ()
main = do
  Just points <- traverse parsePoint3D . lines <$> getContents
  {-
  let hmm = zip (uncurry distance <$> pairs points) $ pairs points
  print $ take 5 $ sortBy (comparing fst) hmm
  -}
  let merged = mergeAll (initCircuits points) $
        take 1000 $ sortBy (comparing $ uncurry distance) $ pairs points
  print $ product $ take 3 $ sortBy (comparing negate) $ S.size <$> merged
