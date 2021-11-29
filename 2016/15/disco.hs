{-# LANGUAGE OverloadedStrings #-}

import GreatWall
import FemtoParsec

type Disc = (Integer, Integer, Integer)

disc :: Parser Disc
disc = do
  _ <- "Disc #"
  n <- unsignedInteger
  _ <- " has "
  modulo <- unsignedInteger
  _ <- " positions; at time=0, it is at position "
  initial <- unsignedInteger
  _ <- "."
  pure (n, modulo, initial)

compile :: Disc -> (Integer, Integer)
compile (timeToDisc, modulo, discAt0) =
  ((-(timeToDisc + discAt0)) `mod` modulo, modulo)

main :: IO ()
main = do
  Just discs <- parseStdin $ many $ lexeme disc
  print $ crt $ compile <$> discs
