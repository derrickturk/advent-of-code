{-# LANGUAGE ScopedTypeVariables #-}

-- https://stackoverflow.com/questions/20257227/how-to-find-all-shortest-paths/35824698#35824698

module AllShortest (
    allShortest
) where

import Control.Monad (guard)
import Data.Maybe (maybeToList)
import Data.List (foldl')
import qualified Data.Map.Strict as M

import qualified Q

labelPaths :: Ord a => a -> (a -> [a]) -> (a -> Bool) -> M.Map a Integer
labelPaths start step goal = go (Q.singleton start) (M.singleton start 0) where
  go q m = case Q.dequeue q of
    Just (s, q') ->
      if goal s
        then m
        else let depth = m M.! s
                 next = step s
                 m' = foldl' see m next -- flip const: keep old
                 see ds s' = M.insertWith (flip const) s' (depth + 1) ds
                 q'' = Q.enqueueList next q'
              in go q'' m'
    Nothing -> m

allShortest :: (Show a, Ord a) => a -> (a -> [a]) -> (a -> Bool) -> [[a]]
allShortest initial oneStep goal = go initial where
  lbls = labelPaths initial oneStep goal
  go start
    | goal start = [[start]]
    | otherwise = do
        next <- oneStep start
        depth <- maybeToList $ lbls M.!? next
        guard $ depth == lbls M.! start + 1
        rest <- go next
        pure (next:rest)
