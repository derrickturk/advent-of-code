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

decode :: String -> [Int]
decode = fmap digitToInt

encode :: [Int] -> String
encode = fmap intToDigit

main :: IO ()
main = do
  input <- decode <$> getLine
  putStrLn $ encode $ take 8 $ iterate fft input !! 100
