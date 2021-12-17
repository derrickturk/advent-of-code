{-# LANGUAGE OverloadedStrings, TupleSections #-}

import FemtoParsec

data State = State { position :: (Int, Int)
                   , velocity :: (Int, Int)
                   } deriving (Show, Eq)

type Target = ((Int, Int), (Int, Int))

launchState :: (Int, Int) -> State
launchState v = State (0, 0) v

ballisticStep :: State -> State
ballisticStep (State (px, py) (vx, vy)) =
  State (px + vx, py + vy) (vx - 1 * signum vx, vy - 1)

targetArea :: Parser Target
targetArea = do
  _ <- "target area: x="
  xmin <- intNum
  _ <- ".."
  xmax <- intNum
  _ <- ", y="
  ymin <- intNum
  _ <- ".."
  ymax <- intNum
  pure ((xmin, xmax), (ymin, ymax))

peakY :: State -> Int
peakY s@(State (_, py) (_, vy))
  | vy <= 0 = py
  | otherwise = peakY $ ballisticStep s

crosses :: Target -> State -> Bool
crosses tgt@((xmin, xmax), (ymin, ymax)) s@(State (px, py) (vx, vy))
  | px <= xmax && px >= xmin && py <= ymax && py >= ymin = True
  | px < xmin && vx <= 0 = False
  | px > xmax && vx >= 0 = False
  | py < ymin && vy <= 0 = False
  | otherwise = crosses tgt (ballisticStep s)

xvRange :: Target -> (Int, Int)
xvRange ((xmin, xmax), _) =
  let xvmin = if xmin >= 0 then 1 else -1
      xvmax = if xmax >= 0 then xmax else xmin
   in (xvmin, xvmax)

validLaunches :: Target -> [State]
validLaunches tgt = do
  let (xvmin, xvmax) = xvRange tgt
  xv <- [xvmin..xvmax]
  filter (crosses tgt) $ launchState . (xv,) <$> [-100..100]

main :: IO ()
main = do
  Just tgt <- parseStdin $ lexeme targetArea
  print $ maximum $ peakY <$> validLaunches tgt
  print $ length $ validLaunches tgt
