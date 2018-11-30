import Text.Read (readMaybe)
import Data.Foldable (foldl')
import Data.Maybe (catMaybes)
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M

data Direction
  = U
  | D
  | L
  | R
  deriving (Eq, Show)

type Grid = M.Map (Int, Int) Int

initialGrid :: Grid
initialGrid = M.fromList [((0, 0), 1)]

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

coordsAtIndex :: Int -> (Int, Int)
coordsAtIndex n = foldl' applyMove (0, 0) $ take n moves

coords :: [(Int, Int)]
coords = scanl applyMove (0, 0) moves

getValue :: (Int, Int) -> State Grid (Maybe Int)
getValue = gets . M.lookup

adjacentSum :: (Int, Int) -> State Grid Int
adjacentSum (x, y) =
  (sum . catMaybes) <$> traverse getValue [ (x, y + 1)
                                          , (x, y - 1)
                                          , (x - 1, y)
                                          , (x + 1, y)
                                          , (x + 1, y + 1)
                                          , (x - 1, y + 1)
                                          , (x + 1, y - 1)
                                          , (x - 1, y - 1)
                                          ]

storeValue :: (Int, Int) -> State Grid Int
storeValue xy = do
  val <- adjacentSum xy
  modify $ M.insert xy val
  pure val

firstBigger :: Int -> Int
firstBigger val = evalState (firstBigger' val $ tail coords) initialGrid where
  firstBigger' val (xy:rest) = do
    written <- storeValue xy
    if written > val
      then pure written
      else firstBigger' val rest
  firstBigger' val [] = error "this shouldn't happen"

main :: IO ()
main = do
  input <- readMaybe <$> getLine
  case input of
    Nothing -> putStrLn "invalid input"
    Just num -> print $ firstBigger num
