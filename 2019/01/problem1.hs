import System.Environment (getArgs)

fuel :: Integer -> Integer
fuel = (subtract 2) . (`div` 3)

totalFuel :: Integer -> Integer
totalFuel = sum . takeWhile (> 0) . tail . iterate fuel

masses :: IO [Integer]
masses = fmap read . lines <$> getContents

main :: IO ()
main = do
  inputs <- masses
  args <- getArgs
  case args of
    ["1"] -> print $ sum $ fuel <$> inputs
    ["2"] -> print $ sum $ totalFuel <$> inputs
    _ -> putStrLn "Usage: problem1 1|2 <input.txt"
