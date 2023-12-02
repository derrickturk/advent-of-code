import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)

firstDigit :: String -> Maybe Int
firstDigit = fmap digitToInt . listToMaybe . dropWhile (not . isDigit)

firstDigitOrWord :: String -> Maybe Int
firstDigitOrWord [] = Nothing
firstDigitOrWord s@(c:rest)
  | isDigit c = Just $ digitToInt c
  | isPrefixOf "one" s = Just 1
  | isPrefixOf "two" s = Just 2
  | isPrefixOf "three" s = Just 3
  | isPrefixOf "four" s = Just 4
  | isPrefixOf "five" s = Just 5
  | isPrefixOf "six" s = Just 6
  | isPrefixOf "seven" s = Just 7
  | isPrefixOf "eight" s = Just 8
  | isPrefixOf "nine" s = Just 9
  | otherwise = firstDigitOrWord rest

firstDigitOrRevWord :: String -> Maybe Int
firstDigitOrRevWord [] = Nothing
firstDigitOrRevWord s@(c:rest)
  | isDigit c = Just $ digitToInt c
  | isPrefixOf (reverse "one") s = Just 1
  | isPrefixOf (reverse "two") s = Just 2
  | isPrefixOf (reverse "three") s = Just 3
  | isPrefixOf (reverse "four") s = Just 4
  | isPrefixOf (reverse "five") s = Just 5
  | isPrefixOf (reverse "six") s = Just 6
  | isPrefixOf (reverse "seven") s = Just 7
  | isPrefixOf (reverse "eight") s = Just 8
  | isPrefixOf (reverse "nine") s = Just 9
  | otherwise = firstDigitOrRevWord rest

calibrationValue :: String -> Maybe Int
calibrationValue s = do
  d1 <- firstDigit s
  d2 <- firstDigit (reverse s)
  return $ d1 * 10 + d2

calibrationValue' :: String -> Maybe Int
calibrationValue' s = do
  d1 <- firstDigitOrWord s
  d2 <- firstDigitOrRevWord (reverse s)
  return $ d1 * 10 + d2

main :: IO ()
main = do
  vals <- lines <$> getContents
  print $ sum <$> traverse calibrationValue vals
  print $ sum <$> traverse calibrationValue' vals
