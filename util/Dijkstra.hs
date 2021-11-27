module Dijkstra (
    costToWin
) where

import Data.List (foldl')

import PQ

update :: (Eq a, Ord a, Ord b) => (b, a) -> PQ (b, a) -> PQ (b, a)
update (cost, item) q = case takeFirstWhere (\(_, x) -> x == item) q of
  Just ((oldCost, _), q') -> if cost == oldCost
    then q
    else insert (min cost oldCost, item) q'
  Nothing -> insert (cost, item) q

-- need Ord for 'tiebreaker' ordering
costToWin :: (Ord a, Num b, Bounded b, Ord b)
          => a
          -> (a -> [(b, a)])
          -> (a -> Bool)
          -> b
costToWin initial validMoves won = costToWin' (fromList [(0, initial)]) where
  costToWin' q = case takeMin q of
    Nothing -> maxBound
    Just ((cost, s), _) | won s -> cost
    Just ((cost, s), rest) ->
      let steps = [(stepCost + cost, s') | (stepCost, s') <- validMoves s]
          q' = foldl' (flip update) rest steps
       in costToWin' q'
