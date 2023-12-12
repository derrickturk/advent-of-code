{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import FemtoParsec

data Dir
  = L
  | R
  deriving (Show, Eq, Ord)

type Maze = M.Map T.Text (T.Text, T.Text)

dir :: Parser Dir
dir = (L <$ char 'L') <|> (R <$ char 'R')

node :: Parser (T.Text, (T.Text, T.Text))
node = do
  from <- lexeme' letters
  _ <- lexeme' "="
  _ <- lexeme' "("
  left <- lexeme' letters
  _ <- lexeme' ","
  right <- lexeme' letters
  _ <- ")"
  return (from, (left, right))

maze :: Parser Maze
maze = M.fromList <$> some (lexeme node)

spec :: Parser ([Dir], Maze)
spec = (,) <$> lexeme (some dir) <*> lexeme' maze

navigate :: Maze -> T.Text -> [Dir] -> [T.Text]
navigate m init path = navigate' m init (cycle path) where
  navigate' _ "ZZZ" _ = []
  navigate' _ _ [] = []
  navigate' m init (L:rest)
    | Just (l, _) <- M.lookup init m = l:navigate' m l rest
    | otherwise = []
  navigate' m init (R:rest)
    | Just (_, r) <- M.lookup init m = r:navigate' m r rest
    | otherwise = []

main :: IO ()
main = do
  Just (path, m) <- parseStdin spec
  print $ length $ navigate m "AAA" path
