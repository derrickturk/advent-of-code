import qualified Data.Map.Strict as M
import Control.Monad (guard)
import Prelude hiding (reverse)

data SparseNode
  = Weakened
  | Infected
  | Flagged
  deriving (Eq, Ord, Show)

newtype Pos = Pos { getPos :: (Int, Int) } deriving (Eq, Ord, Show)
newtype Dir = Dir { getDir :: (Int, Int) } deriving (Eq, Ord, Show)
type World = M.Map Pos SparseNode -- infected node coords
type Carrier = (Pos, Dir)

forward :: Pos -> Dir -> Pos
forward (Pos (x, y)) (Dir (dx, dy)) = Pos (x + dx, y + dy)

left :: Dir -> Dir
left (Dir (0, 1)) = Dir (1, 0)
left (Dir (-1, 0)) = Dir (0, 1)
left (Dir (0, -1)) = Dir (-1, 0)
left (Dir (1, 0)) = Dir (0, -1)
left _ = error "bad Dir!"

-- I'm so lazy, 3 lefts make a right
right :: Dir -> Dir
right = left . left . left

reverse :: Dir -> Dir
reverse (Dir (x, y)) = Dir (-x, -y)

step :: Carrier -> World -> (Carrier, World, Bool)
step (p, d) world
  = case M.lookup p world of
      Just Weakened -> ((forward p d, d), M.insert p Infected world, True)
      Just Infected -> let d' = right d
                        in ((forward p d', d'), M.insert p Flagged world, False)
      Just Flagged -> let d' = reverse d
                       in ((forward p d', d'), M.delete p world, False)
      Nothing -> let d' = left d
                  in ((forward p d', d'), M.insert p Weakened world, False)

grid :: [String] -> Maybe (Int, Int, World)
grid rows = do
  rows' <- traverse parseRow rows
  let lens = length <$> rows'
  guard $ not (null lens) && all (== head lens) lens
  let nR = length lens
      nC = head lens
  guard $ odd nR && odd nC
  let xOrigin = -(nC `div` 2)
      yOrigin = -(nR `div` 2)
      infected = M.fromList
        [ (Pos (j, i), Infected) | (i, row) <- zip [yOrigin..] rows'
                                 , (j, True) <- zip [xOrigin..] row
        ]
  pure (nR, nC, infected)
  where
    parseRow = traverse parseCell
    parseCell '#' = Just True
    parseCell '.' = Just False
    parseCell _ = Nothing

main :: IO ()
main = do
  Just (_, _, world) <- grid . lines <$> getContents
  let carrier = (Pos (0, 0), Dir (0, -1))
  print $ length $ filter (\(_, _, b) -> b) $ take 10000001 $
    iterate (\(c, w, _) -> step c w) (carrier, world, False)
