import Data.Char (digitToInt, intToDigit)

fftDigit :: [Int] -> [Int] -> Int
fftDigit input pat =
  let pat' = drop 1 $ cycle pat
      total = sum $ zipWith (*) input pat'
   in abs $ total `rem` 10

fft :: [Int] -> [Int]
fft input = fftDigit input <$> patternsFor input where
  patternsFor = patternsFor' 0
  patternsFor' _ [] = []
  patternsFor' n (_:rest) = patternFor n : patternsFor' (n + 1) rest

patternFor :: Int -> [Int]
patternFor n =
  replicate (n + 1) 0 <>
  replicate (n + 1) 1 <>
  replicate (n + 1) 0 <>
  replicate (n + 1) (-1)

fastBackHalf :: [Int] -> [Int]
fastBackHalf = fmap (`mod` 10) . scanr1 (+)

decode :: String -> [Int]
decode = fmap digitToInt

encode :: [Int] -> String
encode = fmap intToDigit

main :: IO ()
main = do
  input <- decode <$> getLine
  putStrLn $ encode $ take 8 $ iterate fft input !! 100
  let offset = read $ take 7 $ encode input
  if offset < (length input * 10000) `div` 2
    then putStrLn "that dog won't hunt"
    else do
      let input' = drop offset $ concat $ replicate 10000 input
      putStrLn $ encode $ take 8 $ iterate fastBackHalf input' !! 100
