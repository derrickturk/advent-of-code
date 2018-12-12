module Main where

import qualified Data.Text.IO as TIO
import Plants
import Parse

differences :: [Int] -> [Int]
differences xs = 0:zipWith (-) (tail xs) xs

dropTilConvergence :: Eq a => (b -> a) -> [b] -> [b]
dropTilConvergence _ [] = []
dropTilConvergence _ [_] = []
dropTilConvergence f whole@(x:rest@(y:_))
  | f x == f y = whole
  | otherwise = dropTilConvergence f rest

estimateFuture :: [Int] -> Int -> Int
estimateFuture xs i =
  let third (_, _, x) = x
      trend = dropTilConvergence third $ zip3 [0..] xs (differences xs)
      (j, val, diff) = head trend
  in val + (i - j) * diff

main :: IO ()
main = do
  parsed <- parse (only setup) "stdin" <$> TIO.getContents
  case parsed of
    Left e -> putStrLn $ parseErrorPretty e
    Right (ps, rules) -> do
      let steps = iterateRules (compileRules rules) ps
          filledPots = sum . plantPots <$> steps
      print $ filledPots !! 20
      print $ estimateFuture filledPots 50000000000
