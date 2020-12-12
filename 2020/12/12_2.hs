import Data.List (foldl')
import Text.Read (readMaybe)

data Direction = N | S | E | W
  deriving (Eq, Show)

data Rotation = L | R
  deriving (Eq, Show)

data Action
  = WayMove Direction Int
  | WayRotate Rotation Int
  | Forward Int
  deriving (Eq, Show)

parseAction :: String -> Maybe Action
parseAction ('N':count) = WayMove N <$> readMaybe count
parseAction ('S':count) = WayMove S <$> readMaybe count
parseAction ('E':count) = WayMove E <$> readMaybe count
parseAction ('W':count) = WayMove W <$> readMaybe count
parseAction ('L':count) = WayRotate L <$> readMaybe count
parseAction ('R':count) = WayRotate R <$> readMaybe count
parseAction ('F':count) = Forward <$> readMaybe count
parseAction _ = Nothing

data Ship = Ship { pos :: (Int, Int)
                 , waypoint :: (Int, Int) -- always relative to ship
                 } deriving (Eq, Show)

initShip :: Ship
initShip = Ship (0, 0) (10, 1)

rads :: Int -> Double
rads = (/ 180.0) . (* pi) . fromIntegral

rotateWay :: Rotation -> Int -> (Int, Int) -> (Int, Int)
rotateWay L degs (x, y) = rotateWay' (rads degs) (x, y)
rotateWay R degs (x, y) = rotateWay' (-rads degs) (x, y)

rotateWay' :: Double -> (Int, Int) -> (Int, Int)
rotateWay' theta (x, y) = (
  round (fromIntegral x * cos theta - fromIntegral y * sin theta),
  round (fromIntegral y * cos theta + fromIntegral x * sin theta))

move :: Direction -> Int -> (Int, Int) -> (Int, Int)
move N n (x, y) = (x, y + n)
move S n (x, y) = (x, y - n)
move E n (x, y) = (x + n, y)
move W n (x, y) = (x - n, y)

action :: Ship -> Action -> Ship
action s (WayMove d n) = s { waypoint = move d n (waypoint s) }
action s (WayRotate r degs) = s { waypoint = rotateWay r degs (waypoint s) }
action s (Forward n) = let (x, y) = pos s
                           (i, j) = waypoint s
                        in s { pos = (x + i * n, y + j * n) }

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
