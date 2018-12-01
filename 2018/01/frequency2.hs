import Text.Read (readMaybe)
import qualified Data.Set as S

parseFrequencies :: String -> Maybe [Int]
parseFrequencies = traverse (readMaybe . filter (/= '+')) . words

totalFrequencies :: [Int] -> [Int]
totalFrequencies = scanl (+) 0 . cycle

firstRepeat :: [Int] -> Int
firstRepeat = go S.empty where
  go seen [] = error "invalid input"
  go seen (x:xs)
    | x `S.member` seen = x
    | otherwise = go (S.insert x seen) xs

main :: IO ()
main = do
  r <- fmap (firstRepeat . totalFrequencies) . parseFrequencies <$> getContents
  case r of
    Nothing -> putStrLn "invalid input"
    Just num -> print num
