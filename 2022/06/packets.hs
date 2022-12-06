import qualified Data.Set as S

{-# INLINE countUnique #-}
countUnique :: Ord a => [a] -> Int
countUnique = S.size . S.fromList

untilSignal :: Ord a => Int -> [a] -> Maybe Int
untilSignal k = go 0 where
  go _ [] = Nothing
  go n xs@(_:rest)
    | countUnique (take k xs) == k = Just (n + k)
    | otherwise = go (n + 1) rest

main :: IO ()
main = do
  input <- getLine
  print $ untilSignal 4 input
  print $ untilSignal 14 input
