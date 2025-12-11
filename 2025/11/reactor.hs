import Data.List (uncons)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

type Reactor = M.Map String [String]

parse :: String -> Maybe Reactor
parse = go M.empty . lines where
  go m [] = pure m
  go m (line:rest) = do
    let toks = words line
    (from, to) <- uncons toks
    (from', ":") <- pure $ break (== ':') from
    let m' = M.insert from' to m
    go m' rest

paths :: Reactor -> String -> String -> [[String]]
paths r from to = do
  next <- fromMaybe [] $ r M.!? from
  if to == next
    then [[from, to]]
    else (from:) <$> paths r next to

main :: IO ()
main = do
  Just r <- parse <$> getContents
  print $ length $ paths r "you" "out"
