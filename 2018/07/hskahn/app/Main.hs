module Main where

import qualified Data.Text.IO as TIO
import Kahn
import Workers
import Parse

workers :: Int
workers = 5

costOffset :: Int
costOffset = 60

main :: IO ()
main = do
  g <- parse graph "stdin" <$> TIO.getContents
  case g of
    Left e -> putStr $ parseErrorPretty e
    Right edges -> do
      case topoSort edges of
        Just nodes -> do
          putStr "topological sort: "
          putStrLn $ nodeLabel <$> nodes
        Nothing -> putStrLn "cycle in graph"
      putStr $ "duration w/ " ++ show workers ++ " workers and cost-offset "
        ++ show costOffset ++ ": "
      print $ duration edges workers costOffset
