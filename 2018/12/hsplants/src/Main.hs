module Main where

import qualified Data.Text.IO as TIO
import Plants
import Parse

main :: IO ()
main = do
  parsed <- parse (only setup) "stdin" <$> TIO.getContents
  case parsed of
    Left e -> putStrLn $ parseErrorPretty e
    Right (ps, rules) -> print $ sum $
      plantPots $ iterateRules (compileRules rules) ps !! 20
