{-# LANGUAGE OverloadedStrings #-}

import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Set as S

import FemtoParsec

type Vec3 = (Integer, Integer, Integer)

infixl 6 +|
(+|) :: Vec3 -> Vec3 -> Vec3
(x0, y0, z0) +| (x1, y1, z1) = (x0 + x1, y0 + y1, z0 + z1)

manhattan :: Vec3 -> Integer
manhattan (x, y, z) = abs x + abs y + abs z

type Particle = (Vec3, Vec3, Vec3)

pos :: Particle -> Vec3 
pos (p, _, _) = p

vel :: Particle -> Vec3 
vel (_, v, _) = v

acc :: Particle -> Vec3 
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

particle :: Parser Particle
particle = do
  _ <- lexeme' "p="
  p <- lexeme' vec3
  _ <- lexeme' ", v="
  v <- lexeme' vec3
  _ <- lexeme' ", a="
  a <- vec3
  pure (p, v, a)

step :: Particle -> Particle
step (p, v, a) = (p +| v +| a, v +| a, a)

collidingStep :: [Particle] -> [Particle]
collidingStep = dropCollisions . fmap step

dropCollisions :: [Particle] -> [Particle]
dropCollisions xs = filter (not . (`S.member` naughty) . pos) xs where
  naughty = twiceSeen $ pos <$> xs

twiceSeen :: Ord a => [a] -> S.Set a
twiceSeen = go S.empty S.empty where
  go _ twice [] = twice
  go once twice (x:xs)
    | S.member x once = go once (S.insert x twice) xs
    | otherwise = go (S.insert x once) twice xs

untilAllExpanding :: [Particle] -> [Particle]
untilAllExpanding ps =
  let dists = manhattan . pos <$> ps
      ps' = collidingStep ps
      dists' = manhattan . pos <$> ps'
   in if length dists /= length dists'
        then untilAllExpanding ps'
        else if all (uncurry (<)) $ zip dists dists'
          then ps'
          else untilAllExpanding ps'

main :: IO ()
main = do
  Just ps <- parseStdin $ many $ lexeme particle
  print $ fst $ minimumBy (comparing (manhattan . acc . snd)) $
    zip [0::Int ..] ps
  print $ length $ untilAllExpanding ps
  print $ length $ (iterate collidingStep ps !! 1000)
