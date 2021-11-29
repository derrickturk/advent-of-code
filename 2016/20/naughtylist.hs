import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)
import Data.List (sort)

compile :: [(Int, Int)] -> [(Int, Int)]
compile [] = []
compile [x] = [x]
compile (first@(low1, high1):next@((low2, high2):rest))
  | low2 <= high1 = compile $ (low1, max high1 high2):rest
  | otherwise = first:compile next

parseRange :: T.Text -> Maybe (Int, Int)
parseRange line = case T.split (== '-') line of
  [low, high] -> (,) <$> readMaybe (T.unpack low) <*> readMaybe (T.unpack high)
  _ -> Nothing

lowest :: [(Int, Int)] -> Int
lowest = go 0 where
  go n [] = n
  go n ((low, high):rest)
    | n < low = n
    | otherwise = go (high + 1) rest

countExcluded :: [(Int, Int)] -> Int
countExcluded = sum . fmap ((\(low, high) -> high - low + 1))

main :: IO ()
main = do
  Just vals <- traverse parseRange . T.lines <$> TIO.getContents
  let compiled = compile $ sort vals
  print $ lowest $ compiled
  print $ 2^(32 :: Int) - countExcluded compiled
