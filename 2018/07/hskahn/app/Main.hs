module Main where

import qualified Data.Text.IO as TIO
import Kahn
import Parse

main :: IO ()
main = do
  g <- parse graph "stdin" <$> TIO.getContents
  case g of
    Left e -> putStr $ parseErrorPretty e
    Right edges -> case topoSort edges of
      Just nodes -> putStrLn $ nodeLabel <$> nodes
      Nothing -> putStrLn "cycle in graph"
