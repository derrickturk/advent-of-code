module Main where

import Text.Read (readMaybe)
import Data.Ord (comparing)
import Data.Foldable (maximumBy)
import qualified Data.Vector.Unboxed as VU

newtype Stride = Stride { getStride :: Int }
  deriving (Eq, Show)

newtype Serial = Serial { getSerial :: Int }
  deriving (Eq, Show)

-- stupid stupid stupid 1-based coords

to1d :: Stride -> (Int, Int) -> Int
to1d (Stride s) (x, y) = (x - 1) * s + y - 1

to2d :: Stride -> Int -> (Int, Int)
to2d (Stride s) i = let (x, y) = i `divMod` s in (x + 1, y + 1)

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

main :: IO ()
main = do
  input <- readMaybe <$> getLine
  case input of
    Just serial -> do
      let grid = makeGrid (Serial serial) (Stride 300)
      print $ mostPowerfulBox3 (Stride 300) grid
    Nothing -> putStrLn "invalid input"
