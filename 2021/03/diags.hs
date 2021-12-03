import qualified Data.Map.Strict as M

import Data.Ord (comparing)
import Data.List (foldl', transpose, maximumBy, minimumBy)
import Data.Tuple (swap)

bin2Int :: String -> Maybe Int
bin2Int = go 0 where
  go n [] = Just n
  go n ('0':xs) = go (2 * n) xs
  go n ('1':xs) = go (2 * n + 1) xs
  go _ _ = Nothing

-- can't just "comparing snd" - tie break by 1 > 0
modal :: Ord a => [a] -> a
modal = fst . maximumBy (comparing swap) . M.toList
  . foldl' (\m k -> M.insertWith (+) k (1 :: Int) m) M.empty

antimodal :: Ord a => [a] -> a
antimodal = fst . minimumBy (comparing swap) . M.toList
  . foldl' (\m k -> M.insertWith (+) k (1 :: Int) m) M.empty

oxygen :: [String] -> Maybe Int
oxygen xs = go 0 xs where
  go _ [y] = bin2Int y
  go i ys = go (i + 1) $ filter (\y -> y !! i == modal ((!! i) <$> ys)) ys

co2 :: [String] -> Maybe Int
co2 xs = go 0 xs where
  go _ [y] = bin2Int y
  go i ys = go (i + 1) $ filter (\y -> y !! i == antimodal ((!! i) <$> ys)) ys

main :: IO ()
main = do
  diags <- lines <$> getContents
  let Just gamma = bin2Int $ modal <$> transpose diags
      Just epsilon = bin2Int $ antimodal <$> transpose diags
      Just o = oxygen diags
      Just c = co2 diags
  print $ gamma * epsilon
  print $ o * c
