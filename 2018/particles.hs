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

printPositions :: [Position] -> String
printPositions ps =
  let (x_min, x_max, y_min, y_max, set) = foldl' update (0, 0, 0, 0, S.empty) ps
      update (x_min, x_max, y_min, y_max, s) (Position (x, y)) =
        (min x_min x, max x_max x, min y_min y, max y_max y, S.insert (x, y) s)
      makeRow x_min x_max s y = fmap (drawPoint s y) (enumFromTo x_min x_max)
      drawPoint s y x = if S.member (x, y) s
        then '#'
        else ' '
  in unlines $ fmap (makeRow x_min x_max set) (enumFromTo y_min y_max)

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
  input <- traverse parseParticle . lines <$> getContents
  case input of
    Nothing -> putStrLn "invalid input"
    Just particles -> do
      mapM_ (putStrLn . printPositions) $ take 5 $ positions particles
