import Text.Read (readMaybe)
import Data.List (sortBy, groupBy)
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad ((<=<))
import qualified Data.List.NonEmpty as NE

{- I have no idea what the "right" way is to solve this problem,
 - because I have absolutely no desire to become kind of person
 - who knows an anlytical solution to Manhattan Voronoi tesselation.
 -
 - Life's just too damn short, kids.
 -}

newtype Point = Point { getPoint :: (Int, Int) }
  deriving (Show, Eq, Ord)

x :: Point -> Int
x = fst . getPoint

y :: Point -> Int
y = snd . getPoint

manhattan :: Point -> Point -> Int
manhattan (Point (x1, y1)) (Point (x2, y2)) = abs (x2 - x1) + abs (y2 - y1)

data BoundingBox = BoundingBox { left :: Int
                               , top :: Int
                               , right :: Int
                               , bottom :: Int
                               } deriving (Show, Eq, Ord)

boundingBox :: NonEmpty Point -> BoundingBox
boundingBox ps = BoundingBox (minimum $ x <$> ps)
                             (minimum $ y <$> ps)
                             (maximum $ x <$> ps)
                             (maximum $ y <$> ps)

-- son of all fucks, this is stupid
double :: BoundingBox -> BoundingBox
double (BoundingBox l t r b) =
  let width = (r - l + 1)
      height = (b - t + 1) in
      BoundingBox (l - width `div` 2)
                  (t - height `div` 2)
                  (r + width `div` 2)
                  (b + height `div` 2)

interiorPoints :: BoundingBox -> [Point]
interiorPoints (BoundingBox l t r b) = [Point (x, y) | x <- [l..r], y <- [t..b]]

extreme :: BoundingBox -> Point -> Bool
extreme (BoundingBox l t r b) (Point (x, y)) =
  l == x || r == x || t == y || b == y

nearest :: Point -> [Point] -> Maybe Point
nearest p = check . groupBy (manhattanEq p) . sortBy (manhattanOrd p) where
  manhattanOrd o p1 p2 = compare (manhattan o p1) (manhattan o p2)
  manhattanEq o p1 p2 = manhattan o p1 == manhattan o p2
  check ([p]:_) = Just p
  check _ = Nothing

areaNearest :: BoundingBox -> [Point] -> Point -> Int
areaNearest bbox ps p = length $ filter (isNearest p ps) (interiorPoints bbox)
  where isNearest o ps p = nearest p ps == Just o

parse :: String -> Maybe (NonEmpty Point)
parse = NE.nonEmpty <=< traverse parsePoint . lines where
  parsePoint :: String -> Maybe Point
  parsePoint s = case words s of
    [sX@(_:_), sY] -> Point <$> ((,) <$> readMaybe (init sX) <*> readMaybe sY)
    _ -> Nothing

largestArea :: NonEmpty Point -> Int
largestArea pts =
  let bbox = boundingBox pts
      bbox2 = double bbox 
      pts' = NE.toList pts
      nonXPts = filter (not . extreme bbox) pts'
      areas = areaNearest bbox pts' <$> nonXPts
      areas2 = areaNearest bbox2 pts' <$> nonXPts in
      maximum $ fmap fst $ filter (uncurry (==)) $ zip areas areas2

areaNearerThan :: NonEmpty Point -> Int -> Int
areaNearerThan pts dist =
  let (BoundingBox l t r b) = boundingBox pts
      distN = dist `div` length pts
      bbox = BoundingBox (l - distN) (t - distN) (r + distN) (b + distN)
      totalNearer pts dist p = sum (manhattan p <$> pts) < dist in
      length $ filter (totalNearer pts dist) $ interiorPoints bbox

main :: IO ()
main = do
  input <- parse <$> getContents
  case input of
    Just pts -> do
      print $ largestArea pts
      print $ areaNearerThan pts 10000
    Nothing -> putStrLn "invalid input"
