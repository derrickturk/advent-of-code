import Data.List (foldl', scanl', findIndex)

step :: Integer -> Char -> Integer
step x '(' = x + 1
step x ')' = x - 1
step x _ = x

findFloor :: String -> Integer
findFloor = foldl' step 0

floorSequence :: String -> [Integer]
floorSequence = scanl' step 0

main :: IO ()
main = do
  path <- getContents
  print $ findFloor path
  print $ findIndex (< 0) $ floorSequence path
