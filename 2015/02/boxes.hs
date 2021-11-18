{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)

parseBox :: T.Text -> Maybe (Integer, Integer, Integer)
parseBox line = case T.split (== 'x') line of
  [l, w, h] -> (,,) <$> readMaybe (T.unpack l)
                    <*> readMaybe (T.unpack w)
                    <*> readMaybe (T.unpack h)
  _ -> Nothing

neededPaper :: (Integer, Integer, Integer) -> Integer
neededPaper (l, w, h) = let sides = [l * w, w * h, h * l]
                         in sum (map (* 2) sides) + minimum sides

neededRibbon :: (Integer, Integer, Integer) -> Integer
neededRibbon (l, w, h) = let perims = [2 * (l + w), 2 * (w + h), 2 * (h + l)]
                             volume = l * w * h
                          in minimum perims + volume

main :: IO ()
main = do
  Just dims <- traverse parseBox . T.lines <$> TIO.getContents
  print $ sum $ neededPaper <$> dims
  print $ sum $ neededRibbon <$> dims
