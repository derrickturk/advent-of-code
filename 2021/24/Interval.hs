module Interval (
    Interval
  , getInterval
  , interval
  , singleton
  , (./.)
  , (.%.)
  , (.=.)
  , overlaps
  , contains
) where

import Data.List (sort)

newtype Interval = MkInterval { getInterval :: (Int, Int) }
  deriving (Eq, Ord, Show)

interval :: Int -> Int -> Interval
interval a b = MkInterval (min a b, max a b)

singleton :: Int -> Interval
singleton a = MkInterval (a, a)

instance Num Interval where
  MkInterval (a, b) + MkInterval (c, d) = MkInterval (a + c, b + d)
  MkInterval (a, b) - MkInterval (c, d) = MkInterval (a - d, b - c)
  MkInterval (a, b) * MkInterval (c, d) =
    let vals = [a * c, a * d, b * c, b * d]
     in MkInterval (minimum vals, maximum vals)
  negate (MkInterval (a, b)) = MkInterval (-b, -a)
  abs (MkInterval (a, b)) =
    let a' = abs a
        b' = abs b
     in if a' > b' then MkInterval (b', a') else MkInterval (a', b')
  signum (MkInterval (a, b)) = MkInterval (signum a, signum b)
  fromInteger a = MkInterval (fromInteger a, fromInteger a)

-- ignore zeros, they don't happen in practice
infixl 7 ./.
(./.) :: Interval -> Interval -> Interval
MkInterval (a, b) ./. MkInterval (c, d) =
  let vals = [a `div` c, a `div` d, b `div` c, b `div` d]
   in MkInterval (minimum vals, maximum vals)

-- ignore zeros, they don't happen in practice
infixl 7 .%.
(.%.) :: Interval -> Interval -> Interval
MkInterval (a, b) .%. MkInterval (c, d)
  | c /= d = error "unhandled, b/c we don't need it"
  | otherwise =
      let trials = take c [a..b]
          moduli = (`mod` c) <$> trials
       in MkInterval (minimum moduli, maximum moduli)

infix 4 .=.
(.=.) :: Interval -> Interval -> Interval
i@(MkInterval (a, b)) .=. j@(MkInterval (c, d))
  | a == b && b == c && c == d = MkInterval (1, 1)
  | i `overlaps` j = MkInterval (0, 1)
  | otherwise = MkInterval (0, 0)

overlaps :: Interval -> Interval -> Bool
overlaps (MkInterval (a, b)) (MkInterval (c, d)) =
  (a <= d && b >= c) || (c <= b && d >= a)

contains :: Interval -> Int -> Bool
contains (MkInterval (a, b)) n = a <= n && n <= b
