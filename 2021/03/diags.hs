import qualified Data.Map.Strict as M

import Data.Ord (comparing)
import Data.List (foldl', transpose, maximumBy, minimumBy)

bin2Int :: String -> Maybe Int
bin2Int = go 0 where
  go n [] = Just n
  go n ('0':xs) = go (2 * n) xs
  go n ('1':xs) = go (2 * n + 1) xs
  go _ _ = Nothing

modal :: Ord a => [a] -> a
modal = fst . maximumBy (comparing snd) . M.toList
  . foldl' (\m k -> M.insertWith (+) k (1 :: Int) m) M.empty

antimodal :: Ord a => [a] -> a
antimodal = fst . minimumBy (comparing snd) . M.toList
  . foldl' (\m k -> M.insertWith (+) k (1 :: Int) m) M.empty

main :: IO ()
main = do
  diags <- lines <$> getContents
  let Just gamma = bin2Int $ modal <$> transpose diags
      Just epsilon = bin2Int $ antimodal <$> transpose diags
  print $ gamma * epsilon
