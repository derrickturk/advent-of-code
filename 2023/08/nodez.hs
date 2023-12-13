{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldl')
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
  from <- lexeme' alnum
  _ <- lexeme' "="
  _ <- lexeme' "("
  left <- lexeme' alnum
  _ <- lexeme' ","
  right <- lexeme' alnum
  _ <- ")"
  return (from, (left, right))

maze :: Parser Maze
maze = M.fromList <$> some (lexeme node)

spec :: Parser ([Dir], Maze)
spec = (,) <$> lexeme (some dir) <*> lexeme' maze

navigate :: Maze -> T.Text -> [Dir] -> [T.Text]
navigate m init path = navigate' m init (cycle path) where
  -- navigate' _ "ZZZ" _ = []
  navigate' _ _ [] = []
  navigate' m init (L:rest)
    | Just (l, _) <- M.lookup init m = l:navigate' m l rest
    | otherwise = []
  navigate' m init (R:rest)
    | Just (_, r) <- M.lookup init m = r:navigate' m r rest
    | otherwise = []

cyclicDistanceTo :: Maze -> (T.Text -> Bool) -> [Dir] -> T.Text -> Maybe Int
cyclicDistanceTo m p path init =
  let (nav, rest) = splitAfter p $ navigate m init path
      n = length nav
   in if n == length (takeUntil p rest) then Just n else Nothing

navigateMany :: Maze -> [T.Text] -> [Dir] -> [[T.Text]]
navigateMany m inits path = navigate' m inits (cycle path) where
  navigate' _ inits _
    | all (T.isSuffixOf "Z") inits = []
  navigate' _ _ [] = []
  navigate' m inits (L:rest)
    | Just ls <- fmap fst <$> traverse (m M.!?) inits = ls:navigate' m ls rest
    | otherwise = []
  navigate' m inits (R:rest)
    | Just rs <- fmap snd <$> traverse (m M.!?) inits = rs:navigate' m rs rest
    | otherwise = []

starts :: Maze -> [T.Text]
starts = filter (T.isSuffixOf "A") . M.keys

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs)
  | p x = [x]
  | otherwise = x:takeUntil p xs

splitAfter :: (a -> Bool) -> [a] -> ([a], [a])
splitAfter p = go [] where
  go acc [] = (reverse acc, [])
  go acc (x:xs)
    | p x = (reverse (x:acc), xs)
    | otherwise = go (x:acc) xs

main :: IO ()
main = do
  Just (path, m) <- parseStdin spec
  print $ length $ takeUntil (== "ZZZ") $ navigate m "AAA" path
  Just cds <- pure $
    sequence $ cyclicDistanceTo m (T.isSuffixOf "Z") path <$> starts m
  print $ foldl' lcm 1 cds
