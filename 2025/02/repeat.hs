import Data.Maybe (isJust)
import Text.Read (readMaybe)

repeated :: String -> Maybe String
repeated [] = Nothing
repeated [_] = Nothing
repeated s =
  let stringLen = length s
      maxSeqLen = stringLen `div` 2
      trySeqLens = [1..maxSeqLen]
      go [] = Nothing
      go (n:rest) =
        let (prefix, suffix) = splitAt n s
            expected = concat $ replicate (stringLen `div` n - 1) prefix
         in if stringLen `mod` n == 0 && suffix == expected
              then Just prefix
              else go rest
   in go trySeqLens

repeatedInRange :: Int -> Int -> [Int]
repeatedInRange m n = filter (isJust . repeated . show) [m..n]

repeated2 :: String -> Bool
repeated2 s = case length s `divMod` 2 of
  (0, _) -> False
  (half, 0) -> let (pre, suf) = splitAt half s in pre == suf
  (_, _) -> False

repeated2InRange :: Int -> Int -> [Int]
repeated2InRange m n = filter (repeated2 . show) [m..n]

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split d xs = case break (== d) xs of
  (pre, []) -> [pre]
  (pre, _:suf) -> pre:split d suf

parseRanges :: String -> Maybe [(Int, Int)]
parseRanges = traverse (parseRange . split '-') . split ',' where
  parseRange [x, y] = (,) <$> readMaybe x <*> readMaybe y
  parseRange _ = Nothing

main :: IO ()
main = do
  Just ranges <- parseRanges <$> getContents
  print $ sum $ concatMap (uncurry repeated2InRange) ranges
  print $ sum $ concatMap (uncurry repeatedInRange) ranges
