{-# LANGUAGE OverloadedStrings, TupleSections #-}

import Data.Ord (comparing)
import Data.List (foldl', minimumBy, maximumBy, nub, permutations)
import Text.Read (readMaybe)
import Data.Char (isSpace)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Distances = M.Map (T.Text, T.Text) Integer

shortestTour :: Distances -> [T.Text] -> [T.Text]
shortestTour dists = minimumBy (comparing (totalDistance dists)) . permutations

longestTour :: Distances -> [T.Text] -> [T.Text]
longestTour dists = maximumBy (comparing (totalDistance dists)) . permutations

totalDistance :: Distances -> [T.Text] -> Integer
totalDistance dists = fst . foldl' stepDistance (0, Nothing) where
  stepDistance (soFar, Nothing) next = (soFar, Just next)
  stepDistance (soFar, Just current) next =
    (soFar + dists M.! (current, next), Just next)

parseDistance :: T.Text -> Maybe (T.Text, T.Text, Integer)
parseDistance line = case T.split isSpace line of
  [from, "to", to, "=", dist] -> (from, to,) <$> readMaybe (T.unpack dist)
  _ -> Nothing

distanceMap :: [(T.Text, T.Text, Integer)] -> Distances
distanceMap = foldl' step M.empty where
  step m (from, to, dist) =
    M.insert (from, to) dist $ M.insert (to, from) dist m

main :: IO ()
main = do
  Just dists <- traverse parseDistance . T.lines <$> TIO.getContents
  let distMap = distanceMap dists
      cities = nub $ fst <$> M.keys distMap
  print $ totalDistance distMap $ shortestTour distMap cities
  print $ totalDistance distMap $ longestTour distMap cities
