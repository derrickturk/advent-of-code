import Text.Read (readMaybe)

parseNums :: String -> Maybe [Int]
parseNums = mapM readMaybe . tail . words

distance :: Int -> Int -> Int
distance time buttonTime
  | buttonTime >= time = 0
  | otherwise = buttonTime * (time - buttonTime)

waysToWin :: Int -> Int -> Int
waysToWin time record = length $
  takeWhile (> record) $ dropWhile (<= record) $ distance time <$> [0..time]

main :: IO ()
main = do
  Just times <- parseNums <$> getLine
  Just distances <- parseNums <$> getLine
  let races = zip times distances
  print $ product $ uncurry waysToWin <$> races
