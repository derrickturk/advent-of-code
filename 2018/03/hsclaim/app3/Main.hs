module Main where

import Claim
import FastClaim
import ParseClaim
import qualified Data.Text.IO as TIO
import Control.Monad.ST

countContested :: [Claim] -> ST s Int
countContested cs = do
  (g, szX, szY) <- maxBoundsGrid cs
  mapM_ (paintClaim g szX szY) cs
  contestedCount g

main :: IO ()
main = do
  res <- parse claims "stdin" <$> TIO.getContents
  case res of
    Left e -> putStr $ parseErrorPretty e
    Right cs -> print $ runST $ countContested cs
