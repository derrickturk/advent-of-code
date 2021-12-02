import Control.Applicative
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)

data Dir = U | D | L | R deriving (Eq, Show, Ord)

type Cell = Maybe Char
type Pos = (Int, Int)
type Maze = M.Map (Int, Int) Cell
type Racer = (Pos, Dir)

cw :: Dir -> Dir
cw U = R
cw R = D
cw D = L
cw L = U

ccw :: Dir -> Dir
ccw U = L
ccw L = D
ccw D = R
ccw R = U

try :: Pos -> Dir -> Maze -> Maybe (Racer, Maybe Char)
try p d m = do
  (p', g) <- try' p d
  pure ((p', d), g)
  where
    try' (x, y) U = do
      c' <- M.lookup (x, y - 1) m
      pure ((x, y - 1), c')
    try' (x, y) D = do
      c' <- M.lookup (x, y + 1) m
      pure ((x, y + 1), c')
    try' (x, y) L = do
      c' <- M.lookup (x - 1, y) m
      pure ((x - 1, y), c')
    try' (x, y) R = do
      c' <- M.lookup (x + 1, y) m
      pure ((x + 1, y), c')

step :: Racer -> Maze -> Maybe (Racer, Maybe Char)
step (p, d) m = try p d m <|> try p (cw d) m <|> try p (ccw d) m

letters :: Racer -> Maze -> [Char]
letters r m = case step r m of
  Nothing -> []
  Just (r', Nothing) -> letters r' m
  Just (r', Just c) -> c:letters r' m

path :: Racer -> Maze -> [Pos]
path r m = case step r m of
  Nothing -> []
  Just (r'@(p, _), _) -> p:path r' m

parseMaze :: [String] -> Maybe (Racer, Maze)
parseMaze [] = Nothing
parseMaze (first:rest) = case parseRow 0 first of
  first'@[(p, Nothing)] ->
    Just ( (p, D)
         , M.fromList $ concat (first':fmap (uncurry parseRow) (zip [1..] rest))
         )
  _ -> Nothing

parseRow :: Int -> String -> [(Pos, Cell)]
parseRow y = catMaybes . fmap parseCell . zip [0..] where
  parseCell (x, '|') = Just ((x, y), Nothing)
  parseCell (x, '-') = Just ((x, y), Nothing)
  parseCell (x, '+') = Just ((x, y), Nothing)
  parseCell (_, ' ') = Nothing
  parseCell (x, c) = Just ((x, y), Just c)

main :: IO ()
main = do
  Just (racer, maze) <- parseMaze . lines <$> getContents
  putStrLn $ letters racer maze
  print $ 1 + (length $ path racer maze)
