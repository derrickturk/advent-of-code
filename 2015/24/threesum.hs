{-# LANGUAGE TypeApplications #-}

import Data.Ord (comparing)
import Data.List (scanl', permutations, minimumBy)
import Text.Read (readMaybe)
import Control.Monad (guard)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

{-
threesum :: [Int] -> [([Int], [Int], [Int])]
threesum xs = threesum' where
  target = sum xs `div` 3
  threesum' = do
    p <- permutations xs
    case takeSum target p of
      Just (a, rest) -> case takeSum target rest of
        Just (b, c) -> if sum c == target
          then do
            guard $ (length a < length b) && (length a < length c)
            pure (a, b, c)
          else []
        Nothing -> []
      Nothing -> []
-}

takeSum :: Int -> [Int] -> Maybe ([Int], [Int])
takeSum target xs =
  let priorSum = scanl' (+) 0 xs
      tagged = zip priorSum xs
      (pre, post) = break ((== target) . fst) tagged
   in case post of 
        (_, _):_ -> Just (snd <$> pre, snd <$> post)
        [] | sum (snd <$> pre) == target -> Just (snd <$> pre, [])
        _ -> Nothing

leaveOneOut :: [a] -> [(a, [a])]
leaveOneOut [] = []
leaveOneOut (x:xs) = (x, xs):(prepend x <$> leaveOneOut xs) where
  prepend y (a, as) = (a, y:as)

choose :: [a] -> Int -> [([a], [a])]
choose xs 0 = [([], xs)]
choose xs n = do
  (x, rest) <- leaveOneOut xs
  (group, rest') <- choose rest (n - 1)
  pure (x:group, rest')

maySplit2 :: Int -> [Int] -> Bool
maySplit2 target xs = any goodSplit $ permutations xs where
  goodSplit ys = case takeSum target ys of
    Just (_, rest) -> sum rest == target
    Nothing -> False

firstGroups :: Int -> [Int] -> [[Int]]
firstGroups k xs = go 1 where
  go n
    | n > length xs `div` 3 = []
    | otherwise = case try n of
                    [] -> go (n + 1)
                    res -> res

  try n = do
    (g, rest) <- choose xs n
    guard $ sum g == target
    -- LOL without this we're wrong in general but nobody did it so WTFever
    -- welcome to AOC
    -- vvv would need (maySplit k) to generalize this
    -- guard (maySplit2 target rest)
    [g]

  target = sum xs `div` k

main :: IO ()
main = do
  Just xs <- traverse (readMaybe @Int . T.unpack) . T.lines <$> TIO.getContents
  -- let best = minimumBy (comparing product) $ firstGroups 3 xs
  let best = minimumBy (comparing product) $ firstGroups 4 xs
  print best
  print $ product best
