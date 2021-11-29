import System.Environment (getArgs)
import ChunkyWindows

dragonLen :: [Bool] -> Int -> Int
dragonLen start gens = dragonLens start !! gens

dragonLens :: [Bool] -> [Int]
dragonLens = iterate ((+ 1) . (* 2)) . length

dragon :: [Bool] -> Int -> [Bool]
dragon start size = take size $ snd $ head $ dropWhile (\(n, _) -> n < size) $
  zip (dragonLens start) (iterate dragonStep start)

dragonStep :: [Bool] -> [Bool]
dragonStep xs = xs <> (False:(not <$> reverse xs))

showDragon :: [Bool] -> String
showDragon = map (\b -> if b then '1' else '0')

readDragon :: String -> Maybe [Bool]
readDragon = traverse read01 where
  read01 '0' = Just False
  read01 '1' = Just True
  read01 _ = Nothing

checksum1 :: [Bool] -> [Bool]
checksum1 = map (\[x, y] -> x == y) . chunks 2

-- TODO: precalculate # of steps
checksum :: [Bool] -> [Bool]
checksum = head . dropWhile (even . length) . iterate checksum1

main :: IO ()
main = do
  [input, sz] <- getArgs
  let (Just input') = readDragon input
      sz' = read sz
  putStrLn $ showDragon $ checksum $ dragon input' sz'
