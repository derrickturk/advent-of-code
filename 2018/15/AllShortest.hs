{-# LANGUAGE ScopedTypeVariables #-}

-- https://stackoverflow.com/questions/20257227/how-to-find-all-shortest-paths/35824698#35824698

module AllShortest (
    labelPaths
  , allShortest
) where

import Control.Monad (guard)
import Data.Maybe (maybeToList)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import qualified Q

labelPaths :: Ord a => a -> (a -> [a]) -> (a -> Bool) -> M.Map a Integer
labelPaths start step goal =
  go (Q.singleton start) (M.singleton start 0) S.empty where
    go q m seen = case Q.dequeue q of
      Just (s, q')
        | goal s -> m
        | S.member s seen -> go q' m seen
        | otherwise ->
            let depth = m M.! s
                next = step s
                m' = foldl' see m next -- flip const: keep old
                see ds s' = M.insertWith (flip const) s' (depth + 1) ds
                seen' = S.insert s seen
                q'' = Q.enqueueList next q'
             in go q'' m' seen'
      Nothing -> m

allShortest :: Ord a => a -> (a -> [a]) -> (a -> Bool) -> [[a]]
allShortest initial oneStep goal = allShortest' lbls initial oneStep goal where
  lbls = labelPaths initial oneStep goal

allShortest' :: Ord a
             => M.Map a Integer
             -> a
             -> (a -> [a])
             -> (a -> Bool)
             -> [[a]]
allShortest' lbls initial oneStep goal = go initial where
  go start
    | goal start = [[]]
    | otherwise = do
        next <- oneStep start
        depth <- maybeToList $ lbls M.!? next
        guard $ depth == lbls M.! start + 1
        rest <- go next
        pure (next:rest)
