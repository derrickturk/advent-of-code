import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List (foldl1')
import qualified Data.Set as S

import ChunkyWindows (chunks')

priority :: Char -> Int
priority c
  | isAsciiLower c = fromEnum c - fromEnum 'a' + 1
  | isAsciiUpper c = fromEnum c - fromEnum 'A' + 27
  | otherwise = 0

dups :: Ord a => [a] -> S.Set a
dups xs = let half = length xs `div` 2
              s1 = S.fromList $ take half xs
              s2 = S.fromList $ drop half xs
           in S.intersection s1 s2


common :: Ord a => [[a]] -> S.Set a
common [] = S.empty
common xs = foldl1' S.intersection $ S.fromList <$> xs

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ sum $ fmap priority $ concatMap S.toList $ dups <$> input
  print $ sum $ fmap priority $ concatMap S.toList $ common <$> chunks' 3 input
