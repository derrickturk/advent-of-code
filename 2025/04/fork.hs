import qualified Data.Set as S

sparseGrid :: Eq a => a -> [[a]] -> S.Set (Int, Int)
sparseGrid = goRows S.empty 0 where
  goRows s _ _ [] = s
  goRows s j x (row:rest) = goRows (goCols s j 0 x row) (j + 1) x rest
  goCols s _ _ _ [] = s
  goCols s j i x (a:as)
    | a == x = goCols (S.insert (i, j) s) j (i + 1) x as
    | otherwise = goCols s j (i + 1) x as

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) =
  [ (x, y - 1)
  , (x + 1, y - 1)
  , (x + 1, y)
  , (x + 1, y + 1)
  , (x, y + 1)
  , (x - 1, y + 1)
  , (x - 1, y)
  , (x - 1, y - 1)
  ]


neighbors :: S.Set (Int, Int) -> (Int, Int) -> Int
neighbors s p = length $ filter (`S.member` s) $ adjacent p

removable :: S.Set (Int, Int) -> [(Int, Int)]
removable grid = filter ((< 4) . neighbors grid) $ S.elems grid

totalRemovable :: S.Set (Int, Int) -> Int
totalRemovable s =
  let r = removable s
      n = length r
      s' = s S.\\ S.fromList r
   in if n == 0
        then 0
        else n + totalRemovable s'

main :: IO ()
main = do
  grid <- sparseGrid '@' . lines <$> getContents
  print $ length $ removable grid
  print $ totalRemovable grid
