{-# LANGUAGE OverloadedStrings #-}

import Data.List (minimumBy)
import Data.Ord (comparing)

import FemtoParsec

type Vec3 = (Integer, Integer, Integer)

manhattan :: Vec3 -> Integer
manhattan (x, y, z) = abs x + abs y + abs z

pos :: (Vec3, Vec3, Vec3) -> Vec3 
pos (p, _, _) = p

vel :: (Vec3, Vec3, Vec3) -> Vec3 
vel (_, v, _) = v

acc :: (Vec3, Vec3, Vec3) -> Vec3 
acc (_, _, a) = a

vec3 :: Parser Vec3
vec3 = do
  _ <- lexeme' "<"
  x <- lexeme' integer
  _ <- lexeme' ","
  y <- lexeme' integer
  _ <- lexeme' ","
  z <- lexeme' integer
  _ <- ">"
  pure (x, y, z)

particle :: Parser (Vec3, Vec3, Vec3)
particle = do
  _ <- lexeme' "p="
  p <- lexeme' vec3
  _ <- lexeme' ", v="
  v <- lexeme' vec3
  _ <- lexeme' ", a="
  a <- vec3
  pure (p, v, a)

main :: IO ()
main = do
  Just ps <- parseStdin $ many $ lexeme particle
  print $ fst $ minimumBy (comparing (manhattan . acc . snd)) $
    zip [0::Int ..] ps
