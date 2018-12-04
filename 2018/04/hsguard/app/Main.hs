module Main where

import qualified Data.Text.IO as TIO
import Guard
import Parse

main :: IO ()
main = do
  logs <- parse (only logEntries) "stdin" <$> TIO.getContents
  case logs of
    Left e -> putStr $ parseErrorPretty e
    Right Nothing -> putStrLn "invalid log sequence: event before shift"
    Right (Just entries) -> case sleepMinutes entries of
      Just mins -> do
        let (g, _) = maximumValue mins
        case sleepByMinute g entries of
          Just byMin -> let (m, _) = maximumValue byMin in
            print $ getGuardId g * m
          Nothing -> putStrLn "invalid log sequence: wake before sleep"
      Nothing -> putStrLn "invalid log sequence: wake before sleep"
