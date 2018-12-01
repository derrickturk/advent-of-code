import Text.Read (readMaybe)

parseFrequencies :: String -> Maybe [Int]
parseFrequencies = traverse (readMaybe . filter (/= '+')) . words

main :: IO ()
main = do
  total <- fmap sum . parseFrequencies <$> getContents
  case total of
    Nothing -> putStrLn "invalid input"
    Just num -> print num
