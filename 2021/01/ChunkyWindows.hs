module ChunkyWindows (
    chunks
  , chunks'
  , windows
  , windows'
) where

{-# INLINE chunks #-}
chunks :: Int -> [a] -> [[a]]
chunks n xs = case splitAt n xs of
  (first, []) -> [first]
  (first, rest) -> first:chunks n rest

-- "strict" chunks; drop any trailing short chunks
{-# INLINE chunks' #-}
chunks' :: Int -> [a] -> [[a]]
chunks' n = takeWhile ((== n) . length) . chunks n

{-# INLINE windows #-}
windows :: Int -> [a] -> [[a]]
windows _ [] = []
windows n l@(_:xs) = (take n l):(windows n xs)

-- "strict" windows; drop any trailing short windows
{-# INLINE windows' #-}
windows' :: Int -> [a] -> [[a]]
windows' n = takeWhile ((== n) . length) . windows n
