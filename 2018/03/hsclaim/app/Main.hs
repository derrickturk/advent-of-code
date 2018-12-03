module Main where

import Claim
import ParseClaim
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  res <- parse claims "stdin" <$> TIO.getContents
  case res of
    Left e -> putStr $ parseErrorPretty e
    Right cs -> print $ length $
      filter (uncurry $ contested cs) (uncurry boundsUpTo $ maxBounds cs)
