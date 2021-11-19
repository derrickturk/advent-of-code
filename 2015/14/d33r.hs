{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isSpace)
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Deer = (T.Text, Int, Int, Int)

parseDeer :: T.Text -> Maybe Deer
parseDeer line = case T.split isSpace line of
  [ name
    , "can"
    , "fly"
    , speed
    , "km/s"
    , "for"
    , duration
    , "seconds,"
    , "but"
    , "then"
    , "must"
    , "rest"
    , "for"
    , rest
    , "seconds."
    ] -> (,,,) <$> pure name
               <*> readMaybe (T.unpack speed)
               <*> readMaybe (T.unpack duration)
               <*> readMaybe (T.unpack rest)
  _ -> Nothing

deerPos :: Int -> Deer -> Int
deerPos time (_, speed, duration, rest) =
  let perCycle = speed * duration
      cycleWithRest = duration + rest
      fullCycles = time `div` cycleWithRest
      partialCycle = time `rem` cycleWithRest
      partialCycle' = partialCycle `min` duration
   in fullCycles * perCycle + partialCycle' * speed

main :: IO ()
main = do
  Just deer <- traverse parseDeer . T.lines <$> TIO.getContents
  let positions = deerPos 2503 <$> deer
  print $ maximum positions
