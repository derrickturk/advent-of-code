{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldl', maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Tuple (swap)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import ChunkyWindows
import FemtoParsec

problem :: Parser (T.Text, M.Map T.Text T.Text)
problem = do
  template <- lexeme letters
  rules <- M.fromList <$> many (lexeme rule)
  pure (template, rules)
  where
    rule = do
      from <- lexeme letters
      _ <- lexeme "->"
      to <- letters
      pure (from, to)

applyRules :: M.Map T.Text T.Text -> T.Text -> T.Text
applyRules rs t = T.pack $ go (windows 2 $ T.unpack t) where
  go [] = ""
  go ([c]:[]) = [c]
  go (cs@[a, c]:[]) = case M.lookup (T.pack cs) rs of
    Just b -> a:(T.head b):c:[]
    _ -> cs
  go (cs@[a, _]:rest) = case M.lookup (T.pack cs) rs of
    Just b -> a:(T.head b):(go rest)
    _ -> a:(go rest)
  go _ = error "impossible"

modal :: Ord a => [a] -> (a, Integer)
modal = maximumBy (comparing swap) . M.toList
  . foldl' (\m k -> M.insertWith (+) k (1 :: Integer) m) M.empty

antimodal :: Ord a => [a] -> (a, Integer)
antimodal = minimumBy (comparing swap) . M.toList
  . foldl' (\m k -> M.insertWith (+) k (1 :: Integer) m) M.empty

main :: IO ()
main = do
  Just (tmp, rules) <- parseStdin problem
  let tenth = iterate (applyRules rules) tmp !! 10
      (_, most) = modal $ T.unpack tenth
      (_, least) = antimodal $ T.unpack tenth
  print $ most - least
  let fourtieth = iterate (applyRules rules) tmp !! 40
      (_, most') = modal $ T.unpack fourtieth
      (_, least') = antimodal $ T.unpack fourtieth
  print $ most' - least'
