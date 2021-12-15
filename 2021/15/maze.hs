import Dijkstra

import qualified Data.Map.Strict as M

type Maze = M.Map (Int, Int) Int

parseMaze :: String -> Maze
parseMaze input = M.fromList
  [((x, y), parseChar c) | (y, l) <- zip [0..] (lines input)
                         , (x, c) <- zip [0..] l
  ]
  where
    parseChar c = fromEnum c - fromEnum '0'

validMoves :: Maze -> (Int, Int) -> [(Int, (Int, Int))]
validMoves m (x, y) = do
  next <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  case M.lookup next m of
    Just cost -> pure (cost, next)
    Nothing -> []

biggify :: Maze -> Maze
biggify m = M.fromList $ do
  let ((maxX, maxY), _) = M.findMax m
      width = maxX + 1
      height = maxY + 1
  ((x, y), cost) <- M.toList m
  i <- [0..4]
  j <- [0..4]
  pure ((x + i * width, y + j * height), (cost - 1 + i + j) `mod` 9 + 1)

main :: IO ()
main = do
  maze <- parseMaze <$> getContents
  let (dst, _) = M.findMax maze
  print $ costToWin (0, 0) (validMoves maze) (== dst)
  let bigMaze = biggify maze
      (bigDst, _) = M.findMax bigMaze
  print $ costToWin (0, 0) (validMoves bigMaze) (== bigDst)
