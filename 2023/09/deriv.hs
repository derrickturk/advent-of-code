import Text.Read (readMaybe)

deriv :: [Int] -> [Int]
deriv [] = []
deriv [_] = []
deriv (x1:xs@(x2:_)) = (x2 - x1):deriv xs

derivs :: [Int] -> [[Int]]
derivs = takeWhile (not . all (== 0)) . iterate deriv

parseRecord :: String -> Maybe [Int]
parseRecord = traverse readMaybe . words

umbongo :: [Int] -> Int
umbongo [] = 0
umbongo (x:rest) = x - umbongo rest

main :: IO ()
main = do
  Just records <- traverse parseRecord . lines <$> getContents
  print $ sum $ sum . fmap last . derivs <$> records
  print $ sum $ umbongo . fmap head . derivs <$> records
