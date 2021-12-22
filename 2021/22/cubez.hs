{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (guard)
import Data.List (foldl')

import FemtoParsec

type Range = (Int, Int)

data Cuboid = Cuboid Range Range Range
  deriving (Eq, Show)

data Step = Step Bool Cuboid
  deriving (Eq, Show)

type Volume = [Cuboid]

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
  pure $ Step pwr (Cuboid (xmin, xmax) (ymin, ymax) (zmin, zmax))

disjoint :: Cuboid -> Cuboid -> Bool
disjoint
  (Cuboid (xmin, xmax) (ymin, ymax) (zmin, zmax))
  (Cuboid (xmin', xmax') (ymin', ymax') (zmin', zmax')) =
  xmax < xmin' || xmin > xmax' || ymax < ymin' || ymin > ymax'
    || zmax < zmin' || zmin > zmax'

overlaps :: Cuboid -> Cuboid -> Bool
overlaps x y = not $ disjoint x y

add :: Volume -> Cuboid -> Volume
add v new = go v where
  go [] = [new]
  go (c:cs)
    | disjoint c new = c:go cs
    | otherwise = (remove1 c new) <> go cs

remove :: Volume -> Cuboid -> Volume
remove v minus = go v where
  go [] = []
  go (c:cs)
    | disjoint c minus = c:go cs
    | otherwise = (remove1 c minus) <> go cs

remove1 :: Cuboid -> Cuboid -> [Cuboid]
remove1
  (Cuboid (xmin, xmax) (ymin, ymax) (zmin, zmax))
  minus@(Cuboid (xmin', xmax') (ymin', ymax') (zmin', zmax'))
    | xmin' <= xmin && xmax' >= xmax && ymin' <= ymin && ymax' >= ymax
        && zmin' <= zmin && zmax' >= zmax = []
    | otherwise = do
        xdim <- [ (xmin, xmin' - 1)
                , (max xmin xmin', min xmax xmax')
                , (xmax' + 1, xmax)
                ]
        ydim <- [ (ymin, ymin' - 1)
                , (max ymin ymin', min ymax ymax')
                , (ymax' + 1, ymax)
                ]
        zdim <- [ (zmin, zmin' - 1)
                , (max zmin zmin', min zmax zmax')
                , (zmax' + 1, zmax)
                ]
        guard $ valid xdim && valid ydim && valid zdim
        let result = Cuboid xdim ydim zdim
        guard $ disjoint result minus
        pure result
        where
          valid (dmin, dmax) = dmax >= dmin

volume :: Cuboid -> Int
volume (Cuboid (xmin, xmax) (ymin, ymax) (zmin, zmax))
  = (xmax - xmin + 1) * (ymax - ymin + 1) * (zmax - zmin + 1)

runSteps :: [Step] -> Volume
runSteps = foldl' f [] where
  f v (Step True cube) = add v cube
  f v (Step False cube) = remove v cube

traceSteps :: [Step] -> [Volume]
traceSteps = scanl f [] where
  f v (Step True cube) = add v cube
  f v (Step False cube) = remove v cube

validPart1 :: Cuboid -> Bool
validPart1 (Cuboid (xmin, xmax) (ymin, ymax) (zmin, zmax)) =
  -50 <= xmin && xmax <= 50 && -50 <= ymin && ymax <= 50
  && -50 <= zmin && zmax <= 50

validStepPart1 :: Step -> Bool
validStepPart1 (Step _ xs) = validPart1 xs

main :: IO ()
main = do
  Just steps <- parseStdin $ some $ lexeme step
  print $ sum $ volume <$> (runSteps $ filter validStepPart1 steps)
  print $ sum $ volume <$> runSteps steps
  -- print $ traceSteps $ filter validStepPart1 steps
