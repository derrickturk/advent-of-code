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

countingRotate :: Int -> Rotation -> (Int, Int)
countingRotate n (Left, m) = countingRotate' n (-m)
countingRotate n (Right, m) = countingRotate' n m

countingRotate' :: Int -> Int -> (Int, Int)
countingRotate' n m =
  case (n + m) `divMod` 100 of
    (wraps, 0)
      | wraps >= 1 -> (0, wraps)
      | otherwise -> (0, 1 + abs wraps)
    (wraps, n')
      | n == 0 && wraps < 0 -> (n', abs wraps - 1)
      | otherwise -> (n', abs wraps)

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

countingZeros :: Int -> [Rotation] -> Int
countingZeros = go 0 where
  go acc _ [] = acc
  go acc n (r:rest) =
    let (n', zeros) = countingRotate n r in go (acc + zeros) n' rest

main :: IO ()
main = do
  Just rots <- traverse parseRotation . lines <$> getContents
  print $ length $ filter (== 0) $ scanl rotate 50 rots
  print $ countingZeros 50 rots
