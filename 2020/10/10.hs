{-# LANGUAGE TypeApplications #-}

import Data.List (sort)
import Text.Read (readMaybe)

-- they define this with an implicit starting 0
diff :: Num a => [a] -> [a]
diff xs = zipWith (-) xs (0:xs)

adapterChain :: (Ord a, Num a) => [a] -> [a]
adapterChain xs = sort $ (3 + maximum xs):xs

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

chains :: (Ord a, Num a) => a -> a -> [a] -> Integer
chains last target []
  | last == target = 1
  | otherwise = 0
chains last target (x:xs)
  | x - last > 3 = 0
  | otherwise = chains x target xs + chains last target xs

main :: IO ()
main = do
  inputs <- traverse (readMaybe @Int) . lines <$> getContents
  case inputs of
    Just xs -> do
      let adapters = adapterChain xs
          diffs = diff adapters
          ones = count (== 1) diffs
          threes = count (== 3) diffs
      print $ ones * threes
      print $ chains 0 (last adapters) adapters
    _ -> putStrLn "invalid input"
