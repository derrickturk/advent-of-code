import System.IO (isEOF)

divides :: [Int] -> Maybe (Int, Int)
divides xs = dividesInto xs xs where
  dividesInto [] _ = Nothing
  dividesInto (x:xs) ys = case filter (\y -> x /= y && x `mod` y == 0) ys of
    [] -> dividesInto xs ys
    [y] -> Just (x, y)
    _ -> Nothing

checksum :: [[Int]] -> Maybe Int
checksum = fmap sum . traverse rowSum where
  rowSum = fmap (uncurry div) . divides

parseRow :: String -> [Int]
parseRow = fmap read . words

readSpreadsheet :: IO [[Int]]
readSpreadsheet = do
  done <- isEOF
  if done
    then pure []
    else (:) <$> (parseRow <$> getLine) <*> readSpreadsheet

main :: IO ()
main = do
  cs <- checksum <$> readSpreadsheet
  case cs of
    Nothing -> putStrLn "invalid input"
    Just s -> print s
