{-# LANGUAGE OverloadedStrings #-}

import Data.Ord (Down(..), comparing)
import Data.List (foldl', sortBy)
import qualified Data.Text as T
import qualified Data.Map as M

import FemtoParsec

line :: Parser (String, Int, String)
line = do
  text <- sepBy "-" letters
  _ <- "-"
  sector <- unsignedIntNum
  _ <- "["
  cs <- letters
  _ <- "]"
  pure (concatMap T.unpack text, sector, T.unpack cs)

count :: Ord a => [a] -> [(a, Int)]
count = M.toList . foldl' see M.empty where
  see counts x = M.insertWith (+) x 1 counts

checksum :: Ord a => Int -> [a] -> [a]
checksum n = take n . fmap fst . sortBy (comparing cmpKey) . count where
  cmpKey (letter, freq) = (Down freq, letter)

decrypt :: (String, Int, String) -> String
decrypt (text, sector, _) = fmap (rot sector) text where
  rot n c = toEnum $ (fromEnum c - fromEnum 'a' + n) `mod` 26 + fromEnum 'a'

main :: IO ()
main = do
  Just input <- parseStdin $ many $ lexeme line
  let valid = filter (\(text, _, cs) -> checksum 5 text == cs) input
  print $ sum $ fmap (\(_, sector, _) -> sector) valid
  mapM_ (\l@(_, sector, _) -> putStrLn (show sector <> ": " <> decrypt l)) valid
