module ChunkyWindows (
    chunks
--, windows
) where

chunks :: Int -> [a] -> [[a]]
chunks n xs = case splitAt n xs of
  (first, []) -> [first]
  (first, rest) -> first:chunks n rest

windows :: Int -> [a] -> [[a]]
windows _ [] = []
windows n l@(_:xs) = (take n l):(windows n xs)
