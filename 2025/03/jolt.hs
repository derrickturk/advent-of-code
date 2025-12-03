import Text.Read (readMaybe)

findMaximum :: Ord a => [a] -> (Int, a)
findMaximum [] = error "empty list"
findMaximum (x:xs) = go 0 x 1 xs where
  go i m _ [] = (i, m)
  go i m j (y:ys)
    | y > m = go j y (j + 1) ys
    | otherwise = go i m (j + 1) ys

maxJoltage :: [Int] -> Int
maxJoltage [] = 0
maxJoltage [_] = 0
maxJoltage ds =
  let (i, tens) = findMaximum $ init ds
      ones = maximum $ drop (i + 1) ds
   in 10 * tens + ones

parseBatteries :: String -> Maybe [Int]
parseBatteries = traverse (readMaybe . pure)

main :: IO ()
main = do
  Just banks <- traverse parseBatteries . lines <$> getContents
  print $ sum $ maxJoltage <$> banks
