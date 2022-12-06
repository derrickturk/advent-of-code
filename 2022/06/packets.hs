import Data.List (nub)

untilSignal :: [Char] -> Maybe Int
untilSignal = go 0 where
  go n (a:rest@(b:c:d:_))
    | length (nub [a, b, c, d]) == 4 = Just (n + 4)
    | otherwise = go (n + 1) rest
  go _ _ = Nothing

untilSignal14 :: [Char] -> Maybe Int
untilSignal14 = go 0 where
  go count (a:rest@(b:c:d:e:f:g:h:i:j:k:l:m:n:_))
    | length (nub [a, b, c, d, e, f, g, h, i, j, k, l, m, n]) == 14 = Just (count + 14)
    | otherwise = go (count + 1) rest
  go _ _ = Nothing

main :: IO ()
main = do
  input <- getLine
  print $ untilSignal input
  print $ untilSignal14 input
