{-# LANGUAGE TypeApplications #-}

import Data.Ord (Down(..), comparing)
import Data.List (foldl', group, sort, sortBy)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M

parseCell :: Char -> Maybe Int
parseCell c
  | c >= '0' && c <= '9' = Just $ fromEnum c - fromEnum '0'
  | otherwise = Nothing

parseCave :: String -> Maybe [[Int]]
parseCave = traverse (traverse parseCell) . lines

gridify :: [[a]] -> [((Int, Int), a)]
gridify rows = [ ((i, j), x) | (j, row) <- zip [0..] rows
                             , (i, x) <- zip [0..] row
               ]

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (i, j) = [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]

lowerThanNeighbors :: M.Map (Int, Int) Int -> (Int, Int) -> Bool
lowerThanNeighbors m p =
  let this = m M.! p
      nbrs = catMaybes [m M.!? n | n <- neighbors p]
   in all (this <) nbrs

labelBasins :: M.Map (Int, Int) Int -> M.Map (Int, Int) Int
labelBasins m = snd $ foldl' seePix (0, M.empty) $ M.toList m where
  seePix (i, bs) (_, 9) = (i, bs)
  seePix (i, bs) (p, _)
    | M.member p bs = (i, bs)
    | otherwise = (i + 1, fill i bs p)
  fill i bs p
    | M.member p bs = bs
    | not $ M.member p m = bs
    | m M.! p == 9 = bs
    | otherwise = foldl' (fill i) (M.insert p i bs) $ neighbors p

basinSizes :: M.Map (Int, Int) Int -> [(Int, Int)]
basinSizes bs = fmap (\xs -> (head xs, length xs)) $ group $ sort $ M.elems bs

main :: IO ()
main = do
  Just caves <- parseCave <$> getContents
  let caves' = M.fromList $ gridify caves
      risky = filter (lowerThanNeighbors caves') $ M.keys caves'
  print $ sum $ ((+ 1) . (caves' M.!)) <$> risky
  print $ product $ fmap snd $ take 3 $
    sortBy (comparing $ Down . snd) $ basinSizes $ labelBasins caves'
