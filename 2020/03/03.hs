import qualified Data.Set as S
import Data.Monoid ((<>))

data Forest = Forest { width :: Int
                     , height :: Int
                     , trees :: S.Set (Int, Int)
                     } deriving Show

-- this is dumb, but I like it
instance Semigroup Forest where
  (Forest w1 h1 t1) <> (Forest w2 h2 t2) =
    Forest (max w1 w2) (h1 + h2) (t1 `S.union` S.map (\(x, y) -> (x, y + h1)) t2)
instance Monoid Forest where
  mempty = Forest 0 0 S.empty

parseForestRow :: String -> Forest
parseForestRow = go S.empty 0 where
  go trees width "" = Forest width 1 trees
  go trees width (c:cs) = if c == '#'
    then go (S.insert (width, 0) trees) (width + 1) cs
    else go trees (width + 1) cs

parseForest :: [String] -> Forest
parseForest = mconcat . fmap parseForestRow 

-- note: if down /= 1, we teleport...
data Trajectory = Trajectory { right :: Int
                             , down :: Int
                             } deriving Show

hasTree :: Forest -> (Int, Int) -> Bool
hasTree (Forest width _ trees) (x, y) = (x `mod` width, y) `S.member` trees

path :: (Int, Int) -> Trajectory -> Forest -> [(Int, Int)]
path (x, y) t@(Trajectory right down) f@(Forest _ height _)
  | y >= height = [(x, y)]
  | otherwise = (x, y):(path (x + right, y + down) t f)

treesOnPath' :: (Int, Int) -> Trajectory -> Forest -> Int
treesOnPath' init t f = length $ filter (hasTree f) $ path init t f

treesOnPath :: Trajectory -> Forest -> Int
treesOnPath = treesOnPath' (0, 0)

main :: IO ()
main = do
  forest <- parseForest . lines <$> getContents
  print $ treesOnPath (Trajectory 3 1) forest
