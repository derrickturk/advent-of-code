module Main where

import Data.Bits (popCount)
import Rope
import Hash

hashInts :: String -> DenseHash
hashInts s = denseHash $ ropeCycles (makeRope 256) (stringToLengths s) 64

hashBitsSet :: DenseHash -> Int
hashBitsSet = sum . fmap popCount

hashInputs :: String -> [String]
hashInputs s = [s ++ "-" ++ show n | n <- [0..(127 :: Int)]]

main :: IO ()
main = sum . fmap (hashBitsSet . hashInts) . hashInputs <$> getLine >>= print
