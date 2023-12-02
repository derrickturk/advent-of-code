{-# LANGUAGE OverloadedStrings #-}
-- shootin' dice w/ elves

import Data.List (foldl')

import FemtoParsec

data Dice = Dice { red :: Int, green :: Int, blue :: Int } deriving Show

infix 4 <:
(<:) :: Dice -> Dice -> Bool
(Dice r1 g1 b1) <: (Dice r2 g2 b2) = r1 <= r2 && g1 <= g2 && b1 <= b2

-- you need to be dicemaxxing
instance Semigroup Dice where
  (Dice r1 g1 b1) <> (Dice r2 g2 b2) = Dice (max r1 r2) (max g1 g2) (max b1 b2)

instance Monoid Dice where
  mempty = Dice 0 0 0

power :: Dice -> Int
power (Dice r g b) = r * g * b

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
  return $ foldl' (flip ($)) mempty entries

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
  print $ (sum . fmap (power . mconcat . snd)) <$> games
