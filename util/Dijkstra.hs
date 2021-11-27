module Dijkstra (
    costToWin
  , statesToWin
) where

import Data.List (foldl')

import PQ

update :: (Eq a, Ord a, Ord b) => (b, a) -> PQ (b, a) -> PQ (b, a)
update (cost, item) q = case takeFirstWhere (\(_, x) -> x == item) q of
  Just ((oldCost, _), q') -> if cost == oldCost
    then q
    else insert (min cost oldCost, item) q'
  Nothing -> insert (cost, item) q

update' :: (Eq a, Ord a, Ord b)
        => (b, a, [a])
        -> PQ (b, a, [a])
        -> PQ (b, a, [a])
update' (cost, item, path) q =
  case takeFirstWhere (\(_, x, _) -> x == item) q of
    Just ((oldCost, _, _), q') -> if cost >= oldCost
      then q
      else insert (cost, item, path) q'
    Nothing -> insert (cost, item, path) q

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

statesToWin :: (Ord a, Num b, Bounded b, Ord b)
            => a
            -> (a -> [(b, a)])
            -> (a -> Bool)
            -> (b, [a])
statesToWin initial validMoves won = (total, reverse path) where
  (total, path) = statesToWin' (fromList [(0, initial, [])])
  statesToWin' q = case takeMin q of
    Nothing -> (maxBound, [])
    Just ((cost, s, p), _) | won s -> (cost, s:p)
    Just ((cost, s, p), rest) ->
      let steps = [(stepCost + cost, s', s:p) | (stepCost, s') <- validMoves s]
          q' = foldl' (flip update') rest steps
       in statesToWin' q'
