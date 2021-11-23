codeIxs :: [(Int, Int)]
codeIxs = concat $ codeBand <$> [1..]

codeBand :: Int -> [(Int, Int)]
codeBand n = let m = n + 1 in [(m - i, i) | i <- [1..n]]

codeNums :: [Integer]
codeNums = iterate step 20151125 where
  step n = n * 252533 `mod` 33554393

codez :: [((Int, Int), Integer)]
codez = zip codeIxs codeNums

main :: IO ()
main = do
  let Just code = lookup (2981, 3075) codez
  print code
