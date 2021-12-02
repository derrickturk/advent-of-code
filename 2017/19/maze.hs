import Control.Applicative
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)

data Cell
  = V
  | H
  | X
  | Ltr Char
  deriving (Eq, Show, Ord)

data Dir = U | D | L | R deriving (Eq, Show, Ord)

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

goal :: Cell -> Maybe Char
goal (Ltr x) = Just x
goal _ = Nothing

try :: Cell -> Pos -> Dir -> Maze -> Maybe (Racer, Maybe Char)
try c p d m = do
  (p', g) <- try' c p d
  pure ((p', d), g)
  where
    try' V (x, y) U = do
      c' <- M.lookup (x, y - 1) m
      pure ((x, y - 1), goal c')
    try' V (x, y) D = do
      c' <- M.lookup (x, y + 1) m
      pure ((x, y + 1), goal c')
    try' V _ _ = Nothing
    try' H (x, y) L = do
      c' <- M.lookup (x - 1, y) m
      pure ((x - 1, y), goal c')
    try' H (x, y) R = do
      c' <- M.lookup (x + 1, y) m
      pure ((x + 1, y), goal c')
    try' H _ _ = Nothing
    -- treat letter like +
    try' _ (x, y) U = do
      c' <- M.lookup (x, y - 1) m
      pure ((x, y - 1), goal c')
    try' _ (x, y) D = do
      c' <- M.lookup (x, y + 1) m
      pure ((x, y + 1), goal c')
    try' _ (x, y) L = do
      c' <- M.lookup (x - 1, y) m
      pure ((x - 1, y), goal c')
    try' _ (x, y) R = do
      c' <- M.lookup (x + 1, y) m
      pure ((x + 1, y), goal c')

step :: Racer -> Maze -> Maybe (Racer, Maybe Char)
step (p, d) m =
  let c = m M.! p
   in case c of
     X -> try c p d m <|> try c p (cw d) m <|> try c p (ccw d) m
     _ -> try c p d m

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
  first'@[(p, V)] ->
    Just ( (p, D)
         , M.fromList $ concat (first':fmap (uncurry parseRow) (zip [1..] rest))
         )
  _ -> Nothing

parseRow :: Int -> String -> [(Pos, Cell)]
parseRow y = catMaybes . fmap parseCell . zip [0..] where
  parseCell (x, '|') = Just ((x, y), V)
  parseCell (x, '-') = Just ((x, y), H)
  parseCell (x, '+') = Just ((x, y), X)
  parseCell (_, ' ') = Nothing
  parseCell (x, c) = Just ((x, y), Ltr c)

main :: IO ()
main = do
  Just (racer, maze) <- parseMaze . lines <$> getContents
  print $ letters racer maze
  print $ path racer maze
