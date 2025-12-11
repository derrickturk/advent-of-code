import Control.Monad (guard)
import Data.List (uncons)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import GHC.Internal.TH.Lib (patSynD)

type Reactor = M.Map String [String]
type InvReactor = M.Map String (S.Set String)

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

pathsBottlenecked :: Reactor -> String -> String -> [[String]]
pathsBottlenecked r from to =
  let valid = reachableFrom (inv r) to
      paths' from to
        | not (from `S.member` valid) = []
        | otherwise = do
            next <- fromMaybe [] $ r M.!? from
            if to == next
              then [[from, to]]
              else (from:) <$> paths' next to
   in paths' from to

inv :: Reactor -> InvReactor
inv = foldl' eachNode M.empty . M.toList where
  eachNode m (from, tos) = foldl' (eachLink from) m tos
  eachLink from m to = M.alter (f from) to m
  f from Nothing = Just $ S.singleton from
  f from (Just froms) = Just $ S.insert from froms

reachableFrom :: InvReactor -> String -> S.Set String
reachableFrom invR node = S.insert node $ case invR M.!? node of
  Just from -> foldl' S.union from (reachableFrom invR <$> S.toList from)
  Nothing -> S.empty

reachableAll :: InvReactor -> InvReactor
reachableAll invR = M.fromList $
  (\k -> (k, reachableFrom invR k)) <$> M.keys invR

bottleneck :: InvReactor -> S.Set String -> S.Set String
bottleneck invR waypoints =
  foldl' S.intersection (M.keysSet invR) $ (invR M.!) <$> S.toList waypoints

pathsFromToThrough :: Reactor -> String -> String -> [String] -> [[String]]
pathsFromToThrough r from to through =
  let r' = reachableAll $ inv r
      through' = S.fromList through
      go cur wp b =
        do
          next <- fromMaybe [] $ r M.!? cur
          guard (next `S.member` b)
          let (wp', b') =
                if next `S.member` wp
                  then (S.delete next wp, bottleneck r' $ S.delete next wp)
                else (wp, b)
          if to == next
            then [[cur, to]]
            else (cur:) <$> go next wp' b'
   in go from through' $ bottleneck r' through'

pathsFromToSeq :: Reactor -> String -> [String] -> [[String]]
pathsFromToSeq _ _ [] = []
pathsFromToSeq r from [to] = pathsBottlenecked r from to
pathsFromToSeq r from (to:tos) = do
  path1 <- pathsBottlenecked r from to
  (init path1 <>) <$> pathsFromToSeq r to tos

main :: IO ()
main = do
  Just r <- parse <$> getContents
  print $ length $ paths r "you" "out"
  -- print $ length $ pathsFromToThrough r "svr" "out" ["dac", "fft"]
  print $ length $ pathsBottlenecked r "svr" "dac"
