import Data.List (foldl')
import Text.Read (readMaybe)

data Gaussian = (:+) Integer Integer
  deriving (Eq, Show)
infix 6 :+

instance Num Gaussian where
  (re1 :+ im1) + (re2 :+ im2) = (re1 + re2) :+ (im1 + im2)
  (re1 :+ im1) - (re2 :+ im2) = (re1 - re2) :+ (im1 - im2)
  (re1 :+ im1) * (re2 :+ im2) =
    (re1 * re2 - im1 * im2) :+ (re1 * im2 + re2 * im1)
  abs _ = error "this is a lawless world!"
  signum _ = error "this is a lawless world!"
  fromInteger = (:+ 0)

(*:) :: Gaussian -> Integer -> Gaussian
(re :+ im) *: n = (re * n :+ im * n)
infixl 7 *:

data Action
  = WayMove Gaussian
  | WayRotate Gaussian
  | Forward Integer
  deriving (Eq, Show)

parseAction :: String -> Maybe Action
parseAction ('N':count) = WayMove . (0 :+) <$> readMaybe count
parseAction ('S':count) = WayMove . (0 :+) .negate <$> readMaybe count
parseAction ('E':count) = WayMove . (:+ 0) <$> readMaybe count
parseAction ('W':count) = WayMove . (:+ 0) . negate <$> readMaybe count
parseAction ('L':degrees) = WayRotate <$> (rotation =<< readMaybe degrees)
parseAction ('R':degrees) = WayRotate <$>
  (rotation . negate =<< readMaybe degrees)
parseAction ('F':count) = Forward <$> readMaybe count
parseAction _ = Nothing

rotation :: Int -> Maybe Gaussian
rotation degs = case divMod degs 90 of
  (n, 0) -> Just (if n < 0
    then cycle [(1 :+ 0), (0 :+ (-1)), ((-1) :+ 0), (0 :+ 1)] !! negate n
    else cycle [(1 :+ 0), (0 :+ 1), ((-1) :+ 0), (0 :+ (-1))] !! n)
  _ -> Nothing

data Ship = Ship { pos :: Gaussian
                 , waypoint :: Gaussian -- always relative to ship
                 } deriving (Eq, Show)

initShip :: Ship
initShip = Ship (0 :+ 0) (10 :+ 1)

action :: Ship -> Action -> Ship
action s (WayMove c) = s { waypoint = waypoint s + c }
action s (WayRotate c) = s { waypoint = waypoint s * c }
action s (Forward n) = s { pos = pos s + waypoint s *: n }

runActions :: Ship -> [Action] -> Ship
runActions = foldl' action

manhattan :: Gaussian -> Gaussian -> Integer
manhattan (re1 :+ im1) (re2 :+ im2) = abs (re1 - re2) + abs (im1 - im2)

main :: IO ()
main = do
  actions' <- traverse parseAction . lines <$> getContents
  case actions' of
    Just actions -> do
      let final = runActions initShip actions
      print $ manhattan (0 :+ 0) $ pos final
    Nothing -> putStrLn "invalid input"
