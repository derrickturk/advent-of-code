import Data.List (nub)

untilSignal :: Eq a => Int -> [a] -> Maybe Int
untilSignal k = go 0 where
  go _ [] = Nothing
  go n xs@(_:rest)
    | length (nub $ take k xs) == k = Just (n + k)
    | otherwise = go (n + 1) rest


main :: IO ()
main = do
  input <- getLine
  print $ untilSignal 4 input
  print $ untilSignal 14 input
