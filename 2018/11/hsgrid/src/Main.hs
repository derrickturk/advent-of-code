module Main where

import Text.Read (readMaybe)
import Data.Ord (comparing)
import Data.Foldable (maximumBy)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

newtype Stride = Stride { getStride :: Int }
  deriving (Eq, Show)

newtype Serial = Serial { getSerial :: Int }
  deriving (Eq, Show)

-- stupid stupid stupid 1-based coords

to1d :: Stride -> (Int, Int) -> Int
to1d (Stride s) (x, y) = (x - 1) * s + y - 1
{-# INLINE to1d #-}

to2d :: Stride -> Int -> (Int, Int)
to2d (Stride s) i = let (x, y) = i `divMod` s in (x + 1, y + 1)
{-# INLINE to2d #-}

cellPower :: Serial -> Stride -> Int -> Int
cellPower (Serial ser) s i =
  let (x, y) = to2d s i
      rackId = x + 10
      hundreds n = n `mod` 1000 `div` 100
  in hundreds ((rackId * y + ser) * rackId) - 5

makeGrid :: Serial -> Stride -> VU.Vector Int
makeGrid ser s@(Stride n) = VU.generate (n * n) (cellPower ser s)

box3Power :: Stride -> VU.Vector Int -> (Int, Int) -> Int
box3Power s vec (x, y) = sum $ (vec VU.!) . to1d s <$> coords where
  coords = [(x', y') | x' <- [x..x+2], y' <- [y..y+2]]

mostPowerfulBox3 :: Stride -> VU.Vector Int -> (Int, Int)
mostPowerfulBox3 s@(Stride n) vec =
  maximumBy (comparing $ box3Power s vec) coords where
    coords = [(x, y) | x <- [1..n-2], y <- [1..n-2]]

-- for the DP array
to3d :: Stride -> Int -> (Int, Int, Int)
to3d (Stride s) i = let x = i `div` s `div` s
                        y = i `mod` (s * s) `div` s
                        z = i `mod` s
                    in (x + 1, y + 1, z + 1)
{-# INLINE to3d #-}

from3d :: Stride -> (Int, Int, Int) -> Int
from3d (Stride s) (x, y, z) = (x - 1) * s * s + (y - 1) * s + z - 1
{-# INLINE from3d #-}

boxPower :: Stride -> VU.Vector Int -> V.Vector Int
boxPower s@(Stride n) vec = dpVec where
  dpVec = V.generate (n * n * n) power
  power i =
    let (x, y, z) = to3d s i
    in if z == 1
      then vec VU.! to1d s (x, y)
      else if x + z > n + 1 || y + z > n + 1
        then minBound
        else let sub = dpVec V.! from3d s (x, y, z - 1)
                 fringeX = [(x + z - 1, y') | y' <- [y..y + z - 1]]
                 -- skip the corner to avoid double-counting
                 fringeY = [(x', y + z - 1) | x' <- [x..x + z - 2]]
                 fringe = sum $ (vec VU.!) . to1d s <$> fringeX ++ fringeY
             in sub + fringe

main :: IO ()
main = do
  input <- readMaybe <$> getLine
  case input of
    Just serial -> do
      let stride = (Stride 300)
          grid = makeGrid (Serial serial) stride
          boxPowers = boxPower stride grid
          maxBoxIdx = V.maxIndex boxPowers
      print $ mostPowerfulBox3 stride grid
      print $ to3d stride maxBoxIdx
      print $ boxPowers V.! maxBoxIdx
    Nothing -> putStrLn "invalid input"
