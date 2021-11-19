{-# LANGUAGE TypeApplications #-}

import System.Environment (getArgs)

rle :: Eq a => [a] -> [(Int, a)]
rle [] = []
rle (x:xs) = go 1 x xs where
  go n y [] = [(n, y)]
  go n y (z:zs)
    | y == z = go (n + 1) y zs
    | otherwise = (n, y):go 1 z zs

rleString :: String -> String
rleString = display . rle where
  display [] = ""
  display ((n, c):rest) = show n <> pure c <> display rest

main :: IO ()
main = do
  [input] <- getArgs
  print $ length $ head $ drop 40 $ iterate rleString input
  print $ length $ head $ drop 50 $ iterate rleString input
