module FastClaim (
    coordIndex
  , indexCoord
  , covered
  , makeGrid
  , maxBoundsGrid
  , paintClaim
  , contestedCount
) where

import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Claim

coordIndex :: Int -> Int -> Int -> Int -> Int
coordIndex _ szY x y = x * szY + y

indexCoord :: Int -> Int -> Int -> (Int, Int)
indexCoord _ szY i = (i `div` szY, i `rem` szY)

covered :: Claim -> [(Int, Int)]
covered (Claim _ l r t b) = (,) <$> [l..r] <*> [t..b]

makeGrid :: Int -> Int -> ST s (MV.MVector s Int)
makeGrid szX szY = MV.replicate (szX * szY) 0

maxBoundsGrid :: [Claim] -> ST s (MV.MVector s Int, Int, Int)
maxBoundsGrid cs = let (x, y) = maxBounds cs
                       szX = x + 1
                       szY = y + 1 in
                       (,,) <$> makeGrid szX szY <*> pure szX <*> pure szY

paintClaim :: MV.MVector s Int -> Int -> Int -> Claim -> ST s ()
paintClaim vec szX szY c =
  mapM_ (MV.modify vec (+ 1) . uncurry (coordIndex szX szY)) (covered c)

contestedCount :: MV.MVector s Int -> ST s Int
contestedCount =
  fmap (V.foldl' (\c x -> if x > 1 then c + 1 else c) 0) . V.freeze
