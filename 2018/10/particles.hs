import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.List (stripPrefix, foldl')
import qualified Data.Set as S

newtype Position = Position { getPosition :: (Int, Int) }
  deriving (Eq, Ord, Show)

newtype Velocity = Velocity { getVelocity :: (Int, Int) }
  deriving (Eq, Ord, Show)

newtype Time = Time { getTime :: Int }
  deriving (Eq, Ord, Show)

data Particle = Particle !Position !Velocity
  deriving (Eq, Ord, Show)

positionAt :: Time -> Position -> Velocity -> Position
positionAt (Time t) (Position (x, y)) (Velocity (x', y')) =
  Position (x + x' * t, y + y' * t)
{-# INLINE positionAt #-}

particlePosition :: Time -> Particle -> Position
particlePosition t (Particle p v) = positionAt t p v
{-# INLINE particlePosition #-}

positions :: [Particle] -> [[Position]]
positions ps = (\t -> particlePosition (Time t) <$> ps) <$> enumFrom 0

averages :: [Particle] -> (Int, Int)
averages ps =
  let (avgToOrigin, avgSpeed, len) = foldl' avg (0, 0, 0) ps
      avg (toO, spd, l) (Particle (Position (x, y)) (Velocity (x', y'))) =
        ( if x' == 0 then toO else toO + (-x) `div` x' `div` len
        , spd + sqrt (fromIntegral x' ** 2 + fromIntegral y' ** 2)
            / fromIntegral len
        , l + 1
        )
  in (avgToOrigin, round avgSpeed)

boundingBox :: [Position] -> (Int, Int, Int, Int)
boundingBox ps = foldl' update (maxBound, minBound, maxBound, minBound) ps where
  update (x_min, x_max, y_min, y_max) (Position (x, y)) =
    (min x_min x, max x_max x, min y_min y, max y_max y)

boundingBoxSize :: [Position] -> (Int, Int)
boundingBoxSize ps = let (x0, x1, y0, y1) = boundingBox ps
                     in (x1 - x0, y1 - y0)
{-# INLINE boundingBoxSize #-}

printPositions :: [Position] -> String
printPositions ps =
  let (x_min, x_max, y_min, y_max, set) =
        foldl' update (maxBound, minBound, maxBound, minBound, S.empty) ps
      update (x_min, x_max, y_min, y_max, s) (Position (x, y)) =
        (min x_min x, max x_max x, min y_min y, max y_max y, S.insert (x, y) s)
      makeRow x_min x_max s y = fmap (drawPoint s y) (enumFromTo x_min x_max)
      drawPoint s y x = if S.member (x, y) s
        then '#'
        else ' '
  in unlines $ (makeRow x_min x_max set) <$> (enumFromTo y_min y_max)

parseParticle :: String -> Maybe Particle
parseParticle s = do
  s' <- stripPrefix "position=<" s
  case reads s' of
    [(x, rest)] -> do
      s'' <- stripPrefix ", " rest
      case reads s'' of
        [(y, rest')] -> do
          s''' <- stripPrefix "> velocity=<" rest'
          case reads s''' of
            [(x', rest'')] -> do
              s'''' <- stripPrefix ", " rest''
              case reads s'''' of
                [(y', _)] -> pure $
                  Particle (Position (x, y)) (Velocity (x', y'))
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

main :: IO ()
main = do
  args <- getArgs
  let nTake = case args of
        [] -> 5
        [n] -> read n
  input <- traverse parseParticle . lines <$> getContents
  case input of
    Nothing -> putStrLn "invalid input"
    Just particles -> do
      let (avgToO, avgSpeed) = averages particles
          skip = if avgSpeed == 0
            then avgToO - 10
            else avgToO - max (30 `div` avgSpeed) 3
          sparse ps = let (w, h) = boundingBoxSize ps
                      in w > 240 || h > 40
      mapM_ showLine $ take nTake $
        dropWhile (sparse . snd) $ drop skip $ zip [0..] (positions particles)
  where
    showLine (i, pos) = do
      print i
      putStrLn $ printPositions pos
