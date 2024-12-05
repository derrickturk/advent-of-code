import Control.Arrow ((***))
import Data.List (foldl', sort)
import qualified Data.Map.Strict as M
import Text.Read (readMaybe)

parseTuple :: String -> Maybe (Int, Int)
parseTuple line = do
  let [x, y] = words line 
  (,) <$> readMaybe x <*> readMaybe y

parseLists :: String -> Maybe ([Int], [Int])
parseLists = fmap unzip . traverse parseTuple . lines

part1 :: [Int] -> [Int] -> Int
part1 as bs = sum $ zipWith (\a b -> abs (a - b)) (sort as) (sort bs)

counts :: Ord a => [a] -> M.Map a Int
counts = foldl' f M.empty where
  f m x = M.insertWith (+) x 1 m

part2 :: [Int] -> [Int] -> Int
part2 as bs =
  let tbl = counts bs
      score x = x * M.findWithDefault 0 x tbl
   in sum $ score <$> as

main = do
  Just (as, bs) <- parseLists <$> getContents
  print $ part1 as bs
  print $ part2 as bs
