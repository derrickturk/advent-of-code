{-# LANGUAGE MagicHash, TupleSections #-}

import GHC.Exts (int2Word#, word2Int#, popCnt#, Int(..))
import Control.Monad (guard)
import System.Environment (getArgs)
import qualified Data.Set as S
import qualified Q

import Dijkstra

{-# INLINE popcount #-}
popcount :: Int -> Int
popcount (I# num) = I# (word2Int# (popCnt# (int2Word# num)))

{-# INLINE open #-}
open :: Int -> (Int, Int) -> Bool
open key (x, y) =
  let num = x * x + 3 * x + 2 * x * y + y + y * y + key
   in even $ popcount num

validMoves :: Int -> (Int, Int) -> [(Int, Int)]
validMoves key (x, y) = do
  (i, j) <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  guard $ i >= 0 && j >= 0 && open key (i, j)
  pure (i, j)

shortestPath :: Int -> (Int, Int) -> (Int, Int) -> (Int, [(Int, Int)])
shortestPath key start finish =
  statesToWin start (fmap (1, ) . validMoves key) (== finish)

{-# INLINE countReachable #-}
countReachable :: Int -> Int -> (Int, Int) -> Int
countReachable key maxDepth start = length $ reachable key maxDepth start

reachable :: Int -> Int -> (Int, Int) -> [(Int, Int)]
reachable key maxDepth start =
  reverse $ reachable' [] S.empty (Q.singleton (0, start)) where
    reachable' r seen q = case Q.dequeue q of
      Nothing -> r
      Just ((depth, coord), q')
        | S.member coord seen -> reachable' r seen q'
        | depth >= maxDepth -> reachable' (coord:r) (S.insert coord seen) q'
        | otherwise -> let q'' = Q.enqueueList ((1 + depth,) <$> steps) q'
                           steps = filter (not . flip S.member seen) $
                             validMoves key coord
                        in reachable' (coord:r) (S.insert coord seen) q''

main :: IO ()
main = do
  [key] <- getArgs
  let key' = read key
  print $ fst $ shortestPath key' (1, 1) (31, 39)
  print $ countReachable key' 50 (1, 1)
