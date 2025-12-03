import Text.Read (readMaybe)

findMaximum :: Ord a => [a] -> (Int, a)
findMaximum [] = error "empty list"
findMaximum (x:xs) = go 0 x 1 xs where
  go i m _ [] = (i, m)
  go i m j (y:ys)
    | y > m = go j y (j + 1) ys
    | otherwise = go i m (j + 1) ys

maxJoltage :: Int -> [Int] -> Int
maxJoltage 0 _ = 0
maxJoltage using ds =
  let l = length ds
      ds' = take (l - using + 1) ds
      (i, digit) = findMaximum ds'
   in if length ds < using
        then error "not enough digits"
        else (10 ^ (using - 1)) * digit + maxJoltage (using - 1) (drop (i + 1) ds)

parseBatteries :: String -> Maybe [Int]
parseBatteries = traverse (readMaybe . pure)

main :: IO ()
main = do
  Just banks <- traverse parseBatteries . lines <$> getContents
  print $ sum $ maxJoltage 2 <$> banks
  print $ sum $ maxJoltage 12 <$> banks
