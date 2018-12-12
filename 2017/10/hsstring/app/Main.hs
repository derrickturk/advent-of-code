module Main where

import Text.Read (readMaybe)
import Data.List.Split (splitOn)
import Rope

main :: IO ()
main = do
  input <- traverse readMaybe . splitOn "," <$> getLine
  case input of
    Just lens -> let r = ropeKnots (makeRope 256) lens
                 in print $ r ! 0 * r ! 1
    Nothing -> putStrLn "invalid input"
