import Data.List (foldl')
import Text.Read (readMaybe)

data Direction = N | S | E | W
  deriving (Eq, Show)

data Rotation = L | R
  deriving (Eq, Show)

data Action
  = AbsMove Direction Int
  | Rotate Rotation Int
  | Forward Int
  deriving (Eq, Show)

parseAction :: String -> Maybe Action
parseAction ('N':count) = AbsMove N <$> readMaybe count
parseAction ('S':count) = AbsMove S <$> readMaybe count
parseAction ('E':count) = AbsMove E <$> readMaybe count
parseAction ('W':count) = AbsMove W <$> readMaybe count
parseAction ('L':count) = Rotate L <$> readMaybe count
parseAction ('R':count) = Rotate R <$> readMaybe count
parseAction ('F':count) = Forward <$> readMaybe count
parseAction _ = Nothing

data Ship = Ship { pos :: (Int, Int)
                 , facing :: Direction
                 } deriving (Eq, Show)

initShip :: Ship
initShip = Ship (0, 0) E

rotate :: Rotation -> Int -> Direction -> Direction
rotate _ 0 d = d
rotate L 90 N = W
rotate L 90 S = E
rotate L 90 E = N
rotate L 90 W = S
rotate R 90 N = E
rotate R 90 S = W
rotate R 90 E = S
rotate R 90 W = N
rotate r degs d
  | degs `mod` 90 == 0 = rotate r (degs - 90) $ rotate r 90 d
  | otherwise = d -- wtf else to do?

move :: Direction -> Int -> (Int, Int) -> (Int, Int)
move N n (x, y) = (x, y + n)
move S n (x, y) = (x, y - n)
move E n (x, y) = (x + n, y)
move W n (x, y) = (x - n, y)

action :: Ship -> Action -> Ship
action s (AbsMove d n) = s { pos = move d n (pos s) }
action s (Rotate r degs) = s { facing = rotate r degs (facing s) }
action s (Forward n) = s { pos = move (facing s) n (pos s) }

runActions :: Ship -> [Action] -> Ship
runActions = foldl' action

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

main :: IO ()
main = do
  actions' <- traverse parseAction . lines <$> getContents
  case actions' of
    Just actions -> do
      let final = runActions initShip actions
      print $ manhattan (0, 0) $ pos final
    Nothing -> putStrLn "invalid input"
