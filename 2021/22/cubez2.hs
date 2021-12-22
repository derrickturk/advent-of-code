{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldl')

import FemtoParsec

type Range = (Int, Int)

data Cuboid = Cuboid Range Range Range
  deriving (Eq, Show)

type SignedCuboid = (Bool, Cuboid)

type Volume = [SignedCuboid]

-- different interpretation than SignedCuboid, slightly
newtype Step = Step { unStep :: SignedCuboid }
  deriving (Eq, Show)

step :: Parser Step
step = do
  pwr <- True <$ "on" <|> False <$ "off"
  _ <- " x="
  xmin <- intNum
  _ <- ".."
  xmax <- intNum
  _ <- ",y="
  ymin <- intNum
  _ <- ".."
  ymax <- intNum
  _ <- ",z="
  zmin <- intNum
  _ <- ".."
  zmax <- intNum
  pure $ Step (pwr, Cuboid (xmin, xmax) (ymin, ymax) (zmin, zmax))

disjoint :: Cuboid -> Cuboid -> Bool
disjoint
  (Cuboid (xmin, xmax) (ymin, ymax) (zmin, zmax))
  (Cuboid (xmin', xmax') (ymin', ymax') (zmin', zmax')) =
  xmax < xmin' || xmin > xmax' || ymax < ymin' || ymin > ymax'
    || zmax < zmin' || zmin > zmax'

intersect :: Cuboid -> Cuboid -> Maybe Cuboid
intersect c1@(Cuboid x y z) c2@(Cuboid x' y' z')
  | disjoint c1 c2 = Nothing
  | otherwise = Just $ Cuboid (int1 x x') (int1 y y') (int1 z z')
  where
    int1 (a0, a1) (b0, b1) = (max a0 b0, min a1 b1)

add :: Volume -> Cuboid -> Volume
add v new = (True, new):go v where
  go [] = []
  go ((b, c):cs) = case intersect c new of
    Just i -> (b, c):(not b, i):go cs
    Nothing -> (b, c):go cs

remove :: Volume -> Cuboid -> Volume
remove v minus = go v where
  go [] = []
  go ((b, c):cs) = case intersect c minus of
    Just i -> (b, c):(not b, i):go cs
    Nothing -> (b, c):go cs

volume :: Cuboid -> Int
volume (Cuboid (xmin, xmax) (ymin, ymax) (zmin, zmax))
  = (xmax - xmin + 1) * (ymax - ymin + 1) * (zmax - zmin + 1)

signedVolume :: SignedCuboid -> Int
signedVolume (True, c) = volume c
signedVolume (False, c) = -(volume c)

totalVolume :: Volume -> Int
totalVolume = sum . fmap signedVolume

runSteps :: [Step] -> Volume
runSteps = foldl' f [] where
  f v (Step (True, cube)) = add v cube
  f v (Step (False, cube)) = remove v cube

traceSteps :: [Step] -> [Volume]
traceSteps = scanl f [] where
  f v (Step (True, cube)) = add v cube
  f v (Step (False, cube)) = remove v cube

validPart1 :: Cuboid -> Bool
validPart1 (Cuboid (xmin, xmax) (ymin, ymax) (zmin, zmax)) =
  -50 <= xmin && xmax <= 50 && -50 <= ymin && ymax <= 50
  && -50 <= zmin && zmax <= 50

validStepPart1 :: Step -> Bool
validStepPart1 (Step (_, c)) = validPart1 c

main :: IO ()
main = do
  Just steps <- parseStdin $ some $ lexeme step
  print $ totalVolume $ runSteps $ filter validStepPart1 steps
  print $ totalVolume $ runSteps steps
