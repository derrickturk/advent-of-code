import Data.Maybe (listToMaybe)

diffN :: Eq a => Int -> [a] -> [a] -> Bool
diffN n xs ys = go 0 n $ zip xs ys where
  go diffs n [] = diffs == n
  go diffs n ((x, y):rest)
    | x == y = go diffs n rest
    | otherwise = if diffs + 1 > n
        then False
        else go (diffs + 1) n rest

whichDiffN :: Eq a => Int -> [[a]] -> Maybe ([a], [a])
whichDiffN n [] = Nothing
whichDiffN n (xs:rest) = case listToMaybe $ filter (diffN n xs) rest of
  Just ys -> Just (xs, ys)
  Nothing -> whichDiffN n rest

zipEq :: Eq a => [a] -> [a] -> [a]
zipEq [] _ = []
zipEq _ [] = []
zipEq (x:xs) (y:ys)
  | x == y = x:zipEq xs ys
  | otherwise = zipEq xs ys

main :: IO ()
main = do
  diff1 <- whichDiffN 1 . words <$> getContents
  case diff1 of
    Nothing -> putStrLn "no matching input"
    Just (x, y) -> do
      putStr "first: "
      putStrLn x
      putStr "second: "
      putStrLn y
      putStr "common: "
      putStrLn $ zipEq x y
