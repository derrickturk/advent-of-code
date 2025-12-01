import Control.Monad (guard)
import Prelude hiding (Left, Right)
import Text.Read (readMaybe)

data Direction
  = Left
  | Right
  deriving Show

type Rotation = (Direction, Int)

rotate :: Int -> Rotation -> Int
rotate n (Left, m) = (n - m) `mod` 100
rotate n (Right, m) = (n + m) `mod` 100

parseRotation :: String -> Maybe Rotation
parseRotation [] = Nothing
parseRotation (dir:rest) = do
  dir' <- case dir of
    'L' -> Just Left
    'R' -> Just Right
    _ -> Nothing
  dist <- readMaybe rest
  guard (dist >= 0)
  pure (dir', dist)

main :: IO ()
main = do
  Just rots <- traverse parseRotation . lines <$> getContents
  print $ length $ filter (== 0) $ scanl rotate 50 rots
