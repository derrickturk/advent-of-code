import ChunkyWindows (windows')

trap :: Char -> Bool
trap '^' = True
trap _ = False

showTrap :: Bool -> Char
showTrap True = '^'
showTrap False = '.'

nextRow :: [Bool] -> [Bool]
nextRow = fmap rule . windows' 3 . (False:) . (<> [False]) where
  rule [True, True, False] = True
  rule [False, True, True] = True
  rule [True, False, False] = True
  rule [False, False, True] = True
  rule _ = False

main :: IO ()
main = do
  row <- getLine
  let row' = fmap trap row
  print $ length $ filter not $ concat $ take 400000 $ iterate nextRow row'
