module Main where

import qualified Data.Text.IO as TIO
import Data.List (foldl')
import Guard
import Parse

sleepiestGuardMinute :: [LogEntry] -> (GuardId, Int, Int)
sleepiestGuardMinute es = foldl' update (GuardId 0, 0, 0) $ guardIds es where
  update (g, m, times) g' = case sleepByMinute g' es >>= maximumValueMaybe of
    Nothing -> (g, m, times)
    Just (m', times') -> if times' > times
      then (g', m', times')
      else (g, m, times)

main :: IO ()
main = do
  logs <- parse (only logEntries) "stdin" <$> TIO.getContents
  case logs of
    Left e -> putStr $ parseErrorPretty e
    Right Nothing -> putStrLn "invalid log sequence: event before shift"
    Right (Just entries) ->
      let (GuardId g, m, _) = sleepiestGuardMinute entries in print $ g * m
