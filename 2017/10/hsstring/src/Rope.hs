module Rope (
    Rope
  , makeRope
  , tieKnot
  , ropeKnots
  , (V.!)
) where

import Data.List (foldl')
import qualified Data.Vector.Unboxed as V

type Rope = V.Vector Int

makeRope :: Int -> Rope
makeRope n = V.generate n id

modSlice :: V.Unbox a => Int -> Int -> V.Vector a -> V.Vector a
modSlice i n vec
  | i + n <= V.length vec = V.slice i n vec
  | otherwise =
      let avail = V.length vec - i
          remain = n - avail
      in V.slice i avail vec <> modSlice 0 remain vec
{-# INLINE modSlice #-}

modIndices :: Int -> Int -> V.Vector Int -> V.Vector Int
modIndices i n vec
  | i + n <= V.length vec = V.enumFromN i n
  | otherwise =
      let avail = V.length vec - i
          remain = n - avail
      in V.enumFromN i avail <> modIndices 0 remain vec
{-# INLINE modIndices #-}

modReverseSlice :: Int -> Int -> V.Vector Int -> V.Vector Int
modReverseSlice i n vec = V.update_ vec
  (modIndices i n vec) (V.reverse $ modSlice i n vec)
{-# INLINE modReverseSlice #-}

tieKnot :: Rope -> Int -> Int -> Int -> (Rope, Int, Int)
tieKnot rope pos skip len =
  ( modReverseSlice pos len rope
  , (pos + len + skip) `mod` V.length rope
  , skip + 1
  )
{-# INLINE tieKnot #-}

ropeKnots :: Rope -> [Int] -> Rope
ropeKnots r = fst3 . foldl' tie1 (r, 0, 0) where
  fst3 (x, _, _) = x
  tie1 (rope, pos, skip) len = tieKnot rope pos skip len
{-# INLINE ropeKnots #-}
