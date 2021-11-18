{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

vowels :: [Char]
vowels = "aeiou"

naughtySubs :: [T.Text]
naughtySubs = ["ab", "cd", "pq", "xy"]

hasRun :: T.Text -> Bool
hasRun t = case T.uncons t of
  Just (c, rest) -> case T.uncons rest of
    Just (c', _) -> c == c' || hasRun rest
    _ -> False
  _ -> False

hasRecurrentPair :: T.Text -> Bool
hasRecurrentPair t = case T.uncons t of
  Just (c1, rest) -> case T.uncons rest of
    Just (c2, rest') ->
      T.isInfixOf (T.pack [c1, c2]) rest' || hasRecurrentPair rest
    _ -> False
  _ -> False

hasSplitRepeat :: T.Text -> Bool
hasSplitRepeat t = case T.uncons t of
  Just (c1, rest) -> case T.uncons rest of
    Just (c2, rest') -> case T.uncons rest' of
      Just (c3, _) -> c1 == c3 || hasSplitRepeat rest
      _ -> False
    _ -> False
  _ -> False

rules1 :: [T.Text -> Bool]
rules1 = [ \line -> T.length (T.filter (`elem` vowels) line) >= 3
         , hasRun
         , \line -> not $ any id (mapply (T.isInfixOf <$> naughtySubs) line)
         ]

rules2 :: [T.Text -> Bool]
rules2 = [ hasRecurrentPair
         , hasSplitRepeat
         ]

mapply :: [a -> b] -> a -> [b]
mapply fs x = map ($ x) fs

main :: IO ()
main = do
  input <- T.lines <$> TIO.getContents
  print $ length $ filter (all id . mapply rules1) input
  print $ length $ filter (all id . mapply rules2) input
