module Main where

import qualified Data.Text.IO as TIO
import Stream
import Parse

main :: IO ()
main = do
  input <- parse (only group) "stdin" <$> TIO.getContents
  case input of
    Left e -> putStr $ parseErrorPretty e
    Right g -> do
      print $ score g
      print $ totalGarbage g
