module Main where

import Claim
import ParseClaim
import qualified Data.Text.IO as TIO

disjointAll :: [Claim] -> [Claim]
disjointAll cs = filter (\c -> all (disjointOrSame c) cs) cs where
  disjointOrSame c1 c2 = claimId c1 == claimId c2 || disjoint c1 c2

main :: IO ()
main = do
  res <- parse claims "stdin" <$> TIO.getContents
  case res of
    Left e -> putStr $ parseErrorPretty e
    Right cs -> case disjointAll cs of
      [c] -> print $ claimId c
      [] -> putStrLn "invalid setup: none disjoint"
      _ -> putStrLn "invalid setup: multiple disjoint"
