import System.IO (isEOF)

minMax :: Ord a => [a] -> (a, a)
minMax [] = error "empty input to minMax"
minMax (x:xs) = go x x xs where
  go l g [] = (l, g)
  go l g (x:xs) = go (min l x) (max g x) xs

checksum :: [[Int]] -> Int
checksum = sum . fmap rowSum where
  rowSum = (uncurry $ flip (-)) . minMax

parseRow :: String -> [Int]
parseRow = fmap read . words

readSpreadsheet :: IO [[Int]]
readSpreadsheet = do
  done <- isEOF
  if done
    then pure []
    else (:) <$> (parseRow <$> getLine) <*> readSpreadsheet

main :: IO ()
main = checksum <$> readSpreadsheet >>= print
