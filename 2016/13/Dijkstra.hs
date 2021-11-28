module Dijkstra (
    costToWin
  , statesToWin
) where

import Data.List (foldl')
import qualified Data.Set as S

import PQ

-- TODO: fancyman fibonacci heap for fast decreaseKey operation
-- TODO: A* and other search strategies?

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
costToWin initial validMoves won = fst $
  costToWin' (fromList [(0, initial)]) S.empty where
    costToWin' q seen = case takeMin q of
      Nothing -> (maxBound, seen)
      Just ((cost, s), _) | won s -> (cost, seen)
      Just ((_, s), rest) | S.member s seen -> costToWin' rest seen
      Just ((cost, s), rest) ->
        let steps = [(stepCost + cost, s') | (stepCost, s') <- validMoves s]
            q' = foldl' (flip update) rest steps
         in costToWin' q' (S.insert s seen)

statesToWin :: (Ord a, Num b, Bounded b, Ord b)
            => a
            -> (a -> [(b, a)])
            -> (a -> Bool)
            -> (b, [a])
statesToWin initial validMoves won = (total, reverse path) where
  (total, path) = fst $ statesToWin' (fromList [(0, initial, [])]) S.empty
  statesToWin' q seen = case takeMin q of
    Nothing -> ((maxBound, []), seen)
    Just ((cost, s, p), _) | won s -> ((cost, s:p), seen)
    Just ((_, s, _), rest) | S.member s seen -> statesToWin' rest seen
    Just ((cost, s, p), rest) ->
      let steps = [(stepCost + cost, s', s:p) | (stepCost, s') <- validMoves s]
          q' = foldl' (flip update') rest steps
       in statesToWin' q' (S.insert s seen)
