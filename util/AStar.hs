module AStar (
    costToWin
  , statesToWin
  , countingSteps
) where

import Data.List (foldl')
import qualified Data.Set as S

import PQ

-- TODO: fancyman fibonacci heap for fast decreaseKey operation

update :: (Eq a, Ord a, Ord b) => (b, b, a) -> PQ (b, b, a) -> PQ (b, b, a)
update (h, cost, item) q = case takeFirstWhere (\(_, _, x) -> x == item) q of
  Just ((oldH, oldCost, _), q') -> if cost >= oldCost || h >= oldH
    then q
    else insert (h, cost, item) q'
  Nothing -> insert (h, cost, item) q

update' :: (Eq a, Ord a, Ord b)
        => (b, b, a, [a])
        -> PQ (b, b, a, [a])
        -> PQ (b, b, a, [a])
update' (h, cost, item, path) q =
  case takeFirstWhere (\(_, _, x, _) -> x == item) q of
    Just ((oldH, oldCost, _, _), q') -> if cost >= oldCost || h >= oldH
      then q
      else insert (h, cost, item, path) q'
    Nothing -> insert (h, cost, item, path) q

-- need Ord for 'tiebreaker' ordering
costToWin :: (Ord a, Num b, Bounded b, Ord b)
          => a
          -> (a -> [(b, a)])
          -> (a -> b)
          -> (a -> Bool)
          -> b
costToWin initial validMoves heuristic won = (\(_, cost, _) -> cost) $
  costToWin' (fromList [(heuristic initial, 0, initial)]) S.empty where
    costToWin' q seen = case takeMin q of
      Nothing -> (maxBound, maxBound, seen)
      Just ((h, cost, s), _) | won s -> (h, cost, seen)
      Just ((_, _, s), rest) | S.member s seen -> costToWin' rest seen
      Just ((_, cost, s), rest) ->
        let steps = [ (stepCost + cost + heuristic s', stepCost + cost, s')
                    | (stepCost, s') <- validMoves s
                    ]
            q' = foldl' (flip update) rest steps
         in costToWin' q' (S.insert s seen)

statesToWin :: (Ord a, Num b, Bounded b, Ord b)
            => a
            -> (a -> [(b, a)])
            -> (a -> b)
            -> (a -> Bool)
            -> (b, [a])
statesToWin initial validMoves heuristic won = (total, reverse path) where
  (_, total, path) = fst $
    statesToWin' (fromList [(heuristic initial, 0, initial, [])]) S.empty
  statesToWin' q seen = case takeMin q of
    Nothing -> ((maxBound, maxBound, []), seen)
    Just ((h, cost, s, p), _) | won s -> ((h, cost, s:p), seen)
    Just ((_, _, s, _), rest) | S.member s seen -> statesToWin' rest seen
    Just ((_, cost, s, p), rest) ->
      let steps = [ (stepCost + cost + heuristic s', stepCost + cost, s', s:p)
                  | (stepCost, s') <- validMoves s
                  ]
          q' = foldl' (flip update') rest steps
       in statesToWin' q' (S.insert s seen)

countingSteps :: (a -> [a]) -> (a -> [(Int, a)])
countingSteps = (zip (repeat 1) .)
