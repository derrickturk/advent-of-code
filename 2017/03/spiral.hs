import Text.Read (readMaybe)
import Data.Foldable (foldl')

data Direction
  = U
  | D
  | L
  | R
  deriving (Eq, Show)

directions :: [Direction]
directions = cycle [R, U, L, D]

duration :: [Int]
duration = 1 : 1 : fmap (+1) duration

moves :: [Direction]
moves = concatMap (uncurry replicate) $ zip duration directions

applyMove :: (Int, Int) -> Direction -> (Int, Int)
applyMove (x, y) U = (x, y + 1)
applyMove (x, y) D = (x, y - 1)
applyMove (x, y) L = (x - 1, y)
applyMove (x, y) R = (x + 1, y)

coordsAt :: Int -> (Int, Int)
coordsAt n = foldl' applyMove (0, 0) $ take (n - 1) moves

manhattan :: (Int, Int) -> Int
manhattan (x, y) = abs x + abs y

main :: IO ()
main = do
  input <- readMaybe <$> getLine
  case input of
    Nothing -> putStrLn "invalid input"
    Just num -> print $ manhattan $ coordsAt num
