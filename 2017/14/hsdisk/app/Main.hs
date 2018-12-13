module Main where

import Data.Bits (popCount, testBit)
import Data.Bool (bool)
import Data.List (foldl')
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Set as S
import Rope
import Hash

hashInts :: String -> DenseHash
hashInts s = denseHash $ ropeCycles (makeRope 256) (stringToLengths s) 64

hashBitsSet :: DenseHash -> Int
hashBitsSet = sum . fmap popCount

hashInputs :: String -> [String]
hashInputs s = [s ++ "-" ++ show n | n <- [0..(127 :: Int)]]

hashToVector :: DenseHash -> VU.Vector Bool
hashToVector = VU.fromList . concatMap hashBits where
  hashBits n = testBit n <$> [7, 6..0]

hashesToMatrix :: [DenseHash] -> V.Vector (VU.Vector Bool)
hashesToMatrix = V.fromList . fmap hashToVector

group :: V.Vector (VU.Vector Bool) -> Int -> Int -> Maybe (S.Set (Int, Int))
group mat i j = do
  b <- mat V.!? i >>= (VU.!? j)
  if b
    then Just $ group' S.empty mat i j
    else Nothing
  where
    group' s mat i j
      | S.member (i, j) s = s
      | i < 0 || i >= V.length mat = s
      | j < 0 || j >= VU.length (mat V.! i) = s
      | not (mat V.! i VU.! j) = s
      | otherwise = foldl' (seeNeighbor mat) (S.insert (i, j) s) [ (i + 1, j)
                                                                 , (i - 1, j)
                                                                 , (i, j + 1)
                                                                 , (i, j - 1)
                                                                 ]
    seeNeighbor mat s (i, j) = group' s mat i j

groups :: V.Vector (VU.Vector Bool) -> [S.Set (Int, Int)]
groups mat = snd $ foldr seeRow (S.empty, []) [0..V.length mat - 1] where
  seeRow i (seen, gs) = foldr (see i) (seen, gs) [0..VU.length (mat V.! i) - 1]
  see i j (seen, gs) = if S.member (i, j) seen
      then (seen, gs)
      else case group mat i j of
        Just g -> (S.union seen g, g:gs)
        Nothing -> (S.insert (i, j) seen, gs)

printMatrix :: V.Vector (VU.Vector Bool) -> String
printMatrix = unlines . V.toList . V.map printVec where
  printVec = VU.toList . VU.map (bool '.' '#')

main :: IO ()
main = do
  hashes <- fmap hashInts . hashInputs <$> getLine
  print $ sum $ hashBitsSet <$> hashes
  print $ length $ groups $ hashesToMatrix hashes
