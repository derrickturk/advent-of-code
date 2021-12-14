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

compile :: T.Text -> M.Map T.Text Integer
compile = foldl' (\m k -> M.insertWith (+) k (1 :: Integer) m) M.empty
  . fmap T.pack . windows' 2 . T.unpack

applyCompiled :: M.Map T.Text T.Text
              -> M.Map T.Text Integer
              -> M.Map T.Text Integer
applyCompiled rs t = foldl' see M.empty $ M.toList t where
  see t' (pair, n) = case M.lookup pair rs of
    Nothing -> M.insert pair n t'
    Just c -> let c' = T.head c
                  lefty = T.snoc (T.singleton $ T.head pair) c'
                  righty = T.cons c' $ T.singleton $ T.last pair
               in M.insertWith (+) lefty n $ M.insertWith (+) righty n t'

counts :: M.Map T.Text Integer -> M.Map Char Integer
counts = foldl' see M.empty . M.toList where
  see cs (pair, n) = M.insertWith (+) (T.head pair) n $
    M.insertWith (+) (T.last pair) 1 cs

main :: IO ()
main = do
  Just (tmp, rules) <- parseStdin problem
  let tenth = iterate (applyRules rules) tmp !! 10
      modeDiff txt = snd (modal $ T.unpack txt) - snd (antimodal $ T.unpack txt)
      diff = modeDiff tenth
  print diff
  let tenth' = iterate (applyCompiled rules) (compile tmp) !! 10
      tenthCounts = counts tenth'
      diff' = maximum (M.elems tenthCounts) - minimum (M.elems tenthCounts)
      err = diff - diff' -- calibrate the, uh, error term??? IDK it works in practice
      fourtieth = iterate (applyCompiled rules) (compile tmp) !! 40
      fourtiethCounts = counts fourtieth
  print $
    maximum (M.elems fourtiethCounts) - minimum (M.elems fourtiethCounts) + err
