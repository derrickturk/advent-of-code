import Data.Char (isDigit, ord)

pairs :: [Int] -> [(Int, Int)]
pairs xs = zip xs (tail xs ++ [head xs])

checksum :: [Int] -> Int
checksum = sum . fmap fst . filter (uncurry (==)) . pairs

parse :: String -> Maybe [Int]
parse = traverse parseChar where
  parseChar c
    | isDigit c = Just $ ord c - ord '0'
    | otherwise = Nothing

main :: IO ()
main = do
  input <- parse <$> getLine
  case input of
    Just digits -> print $ checksum digits
    Nothing -> putStrLn "invalid input"
