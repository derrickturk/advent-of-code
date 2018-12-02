module Main where

import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M

import Reggy
import ParseReggy

main :: IO ()
main = do
  prog <- parse reggy "stdin" <$> TIO.getContents
  case prog of
    Left e -> putStr $ parseErrorPretty e
    Right p -> print $ maximum $ M.elems $ runReggy p
