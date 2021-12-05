import qualified Data.Set as S
import Control.Monad (guard)

newtype Pos = Pos { getPos :: (Int, Int) } deriving (Eq, Ord, Show)
newtype Dir = Dir { getDir :: (Int, Int) } deriving (Eq, Ord, Show)
type World = S.Set Pos -- infected node coords
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

step :: Carrier -> World -> (Carrier, World, Bool)
step (p, d) world
  | S.member p world = let d' = right d
                        in ((forward p d', d'), S.delete p world, False)
  | otherwise = let d' = left d
                 in ((forward p d', d'), S.insert p world, True)

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
      infected = S.fromList [ Pos (j, i) | (i, row) <- zip [yOrigin..] rows'
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
  print $ length $ filter (\(_, _, b) -> b) $ take 10001 $
    iterate (\(c, w, _) -> step c w) (carrier, world, False)
