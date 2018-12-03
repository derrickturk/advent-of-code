module Claim (
    Claim(..)
  , contains
  , maxBounds
  , boundsUpTo
  , claimCount
  , contested
  , disjoint
  , overlaps
) where

import Data.List (foldl')

data Claim = Claim { claimId :: Int
                   , left :: Int
                   , right :: Int
                   , top :: Int
                   , bottom :: Int
                   } deriving Show

contains :: Claim -> Int -> Int -> Bool
contains (Claim _ l r t b) x y = x >= l && x <= r && y >= t && y <= b

maxBounds :: [Claim] -> (Int, Int)
maxBounds = foldl' maxXY (0, 0) where
  maxXY (x, y) (Claim _ _ r _ b) = (max x r, max y b)

boundsUpTo :: Int -> Int -> [(Int, Int)]
boundsUpTo x y = (,) <$> [0..x] <*> [0..y]

claimCount :: [Claim] -> Int -> Int -> Int
claimCount cs x y = sum $ fmap (\c -> if contains c x y then 1 else 0) cs

contested :: [Claim] -> Int -> Int -> Bool
contested = go 0 where
  go n [] _ _ = n > 1
  go n (c:cs) x y = if n > 1
    then True
    else if contains c x y
      then go (n + 1) cs x y
      else go n cs x y

disjoint :: Claim -> Claim -> Bool
disjoint (Claim _ l1 r1 t1 b1) (Claim _ l2 r2 t2 b2) =
  r1 < l2 || r2 < l1 || b1 < t2 || b2 < t1

overlaps :: Claim -> Claim -> Bool 
overlaps c1 c2 = not $ disjoint c1 c2
