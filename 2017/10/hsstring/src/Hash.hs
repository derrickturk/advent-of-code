module Hash (
    DenseHash
  , stringToLengths
  , ropeCycles
  , denseHash
  , hashHex
  , ropeHash
) where

import Data.Char (ord)
import Data.Bits (xor)
import Data.List (unfoldr)
import qualified Data.Vector.Unboxed as V
import Text.Printf
import Rope

type DenseHash = [Int]

stringToLengths :: String -> [Int]
stringToLengths = (++ [17, 31, 73, 47, 23]) . fmap ord
{-# INLINE stringToLengths #-}

ropeCycles :: Rope -> [Int] -> Int -> Rope
ropeCycles r ls n = ropeKnots r (concat $ replicate n ls)
{-# INLINE ropeCycles #-}

denseHash :: Rope -> DenseHash
denseHash = unfoldr hashChunk where
  hashChunk r
    | V.null r = Nothing
    | otherwise = let (chunk, rest) = V.splitAt 16 r
                  in Just (V.foldl1' xor chunk, rest)

hashHex :: DenseHash -> String
hashHex = concatMap (printf "%02x")
{-# INLINE hashHex #-}

ropeHash :: Int -> Int -> String -> String
ropeHash size cycles str =
  let rope = makeRope size
      lens = stringToLengths str
  in hashHex $ denseHash $ ropeCycles rope lens cycles
{-# INLINE ropeHash #-}
