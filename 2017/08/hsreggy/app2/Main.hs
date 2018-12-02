module Main where

import qualified Data.Text.IO as TIO

import Reggy
import ParseReggy

main :: IO ()
main = do
  prog <- parse reggy "stdin" <$> TIO.getContents
  case prog of
    Left e -> putStr $ parseErrorPretty e
    Right p -> print $ fst $ runReggyMaximum p
