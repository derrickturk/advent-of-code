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

main :: IO ()
main = do
  inputs <- traverse (readMaybe @Int) . lines <$> getContents
  case inputs of
    Just xs -> do
      let diffs = diff $ adapterChain xs
          ones = count (== 1) diffs
          threes = count (== 3) diffs
      print $ ones * threes
    _ -> putStrLn "invalid input"
