import Data.Char (digitToInt, isDigit)
import Data.Maybe (listToMaybe)

firstDigit :: String -> Maybe Int
firstDigit = fmap digitToInt . listToMaybe . dropWhile (not . isDigit)

calibrationValue :: String -> Maybe Int
calibrationValue s = do
  d1 <- firstDigit s
  d2 <- firstDigit (reverse s)
  return $ d1 * 10 + d2

main :: IO ()
main = do
  vals <- lines <$> getContents
  print $ sum <$> traverse calibrationValue vals
