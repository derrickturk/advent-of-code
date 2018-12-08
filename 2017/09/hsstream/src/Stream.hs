module Stream (
    Group(..)
  , score
  , totalGarbage
) where

import Data.Either (partitionEithers, rights)
import qualified Data.Text as T

data Group = Group [Either T.Text Group]
  deriving Show

score :: Group -> Int
score = score' 1

score' :: Int -> Group -> Int
score' n (Group []) = n
score' n (Group gs) = n + (sum $ score' (n + 1) <$> rights gs)

totalGarbage :: Group -> Int
totalGarbage (Group gs) = let (garbage, groups) = partitionEithers gs in
  sum (T.length <$> garbage) + sum (totalGarbage <$> groups)
