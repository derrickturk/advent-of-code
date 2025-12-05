import Data.List (sortOn)
import Prelude hiding (Left, Right, min, max)
import Text.Read (readMaybe)

-- https://en.wikipedia.org/wiki/Interval_tree

type Interval = (Int, Int)

data IntervalTree
  = Node Int IntervalTree IntervalTree [Interval] [Interval]
  | Empty
  deriving Show

data IntervalCompare
  = Left
  | Right
  | Contains
  deriving Show

-- interval is [Left, Right, Center] of point
intervalCompare :: Interval -> Int -> IntervalCompare
intervalCompare (min, max) x
  | x < min = Right
  | x > max = Left
  | otherwise = Contains

partition :: Int -> [Interval] -> ([Interval], [Interval], [Interval])
partition = go [] [] [] where
  go lefts rights overlap _ [] = (lefts, rights, overlap)
  go lefts rights overlap x (i:is) = case intervalCompare i x of
    Left -> go (i:lefts) rights overlap x is
    Right -> go lefts (i:rights) overlap x is
    Contains -> go lefts rights (i:overlap) x is

fromList :: [Interval] -> IntervalTree
fromList [] = Empty
fromList is =
  let (min, max) = (minimum $ fst <$> is, maximum $ snd <$> is)
      center = min + (max - min) `div` 2
      (lefts, rights, overlap) = partition center is
   in Node center
           (fromList lefts)
           (fromList rights)
           (sortOn fst overlap)
           (sortOn snd overlap)

contains :: IntervalTree -> Int -> Bool
contains Empty _ = False
contains (Node c l r byMin byMax) x
  | x < c = contains l x || any ((<= x) . fst) byMin
  | x > c =  contains r x || any ((>= x) . snd) byMax
  | otherwise = not $ null byMin

data ProblemSpec
  = ProblemSpec [Interval] [Int]
  deriving Show

parse :: [String] -> Maybe ProblemSpec
parse ls = case break null ls of
  (intervals, "":pts) ->
    ProblemSpec <$> traverse parseI intervals <*> traverse readMaybe pts
  _ -> Nothing
  where parseI interval = case break (== '-') interval of
          (l, '-':r) -> (,) <$> readMaybe l <*> readMaybe r
          _ -> Nothing

main :: IO ()
main = do
  Just (ProblemSpec is pts) <- parse . lines <$> getContents
  let tree = fromList is
  print $ length $ filter (contains tree) pts
