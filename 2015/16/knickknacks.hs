{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

import FemtoParsec

type Sue = [(T.Text, Integer)]

known :: Sue
known = [ ("children", 3)
        , ("cats", 7)
        , ("samoyeds", 2)
        , ("pomeranians", 3)
        , ("akitas", 0)
        , ("vizslas", 0)
        , ("goldfish", 5)
        , ("trees", 3)
        , ("cars", 2)
        , ("perfumes", 1)
        ]

matches :: Sue -> Sue -> Bool
matches spec = all valid where
  valid (item, val) = case lookup item spec of
    Just val' -> val == val'
    _ -> True

matches' :: Sue -> Sue -> Bool
matches' spec = all valid where
  valid (item, val) = case lookup item spec of
    Just val' -> case item of
      "cats" -> val > val'
      "trees" -> val > val'
      "pomeranians" -> val < val'
      "goldfish" -> val < val'
      _ -> val == val'
    _ -> True

sue :: Parser (Int, Sue)
sue = do
  _ <- lexeme "Sue"
  n <- fromIntegral <$> integer
  _ <- lexeme $ char ':'
  items <- sepBy (lexeme $ char ',') item
  pure (n, items)
  where
    item :: Parser (T.Text, Integer)
    item = (,) <$> (letters <* lexeme (char ':'))
               <*> integer

main :: IO ()
main = do
  Just sues <- parseStdin (some $ lexeme sue)
  print $ fst <$> filter (matches known . snd) sues
  print $ fst <$> filter (matches' known . snd) sues
