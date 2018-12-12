module Main where

import Hash

main :: IO ()
main = getLine >>= putStrLn . ropeHash 256 64
