{-# LANGUAGE OverloadedStrings #-}
-- shootin' dice w/ elves

import Data.List (foldl')

import FemtoParsec

data Dice = Dice { red :: Int, green :: Int, blue :: Int } deriving Show

infix 4 <:
(<:) :: Dice -> Dice -> Bool
(Dice r1 g1 b1) <: (Dice r2 g2 b2) = r1 <= r2 && g1 <= g2 && b1 <= b2

color :: Parser (Int -> Dice -> Dice)
color =  ((\v d -> d { red = v }) <$ "red")
     <|> ((\v d -> d { green = v }) <$ "green")
     <|> ((\v d -> d { blue = v }) <$ "blue")

entry :: Parser (Dice -> Dice)
entry = do
  n <- unsignedIntNum
  " "
  c <- color
  return $ c n

reveal :: Parser Dice
reveal = do
  entries <- sepBy ", " entry
  return $ foldl' (flip ($)) (Dice 0 0 0) entries

game :: Parser (Int, [Dice])
game = do
  "Game "
  n <- unsignedIntNum
  ": "
  reveals <- sepBy "; " reveal
  return $ (n, reveals)

main :: IO ()
main = do
  games <- parseStdin (some (lexeme' game))
  print $ (sum . fmap fst . filter (all (<: Dice 12 13 14) . snd)) <$> games
