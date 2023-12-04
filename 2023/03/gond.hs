import Data.Char (digitToInt, isDigit)
import Data.List (foldl')
import qualified Data.Map.Strict as M

data Cell n
  = Number n
  | Symbol Char
  deriving (Show, Eq, Ord)

-- Number = single digit
type World = M.Map (Int, Int) (Cell Int)

{-
data SpanCell
  = Number (Int, Int)
  | Symbol Char
  deriving (Show, Eq, Ord)
-}

-- Number = span with length
type SpanWorld = M.Map (Int, Int) (Cell (Int, Int))

parse :: [String] -> World
parse = foldl' parseRow M.empty . zip [0..] where
  parseRow w (i, row) = foldl' (parseCell i) w (zip [0..] row)
  parseCell i w (j, c)
    | c == '.' = w
    | isDigit c = M.insert (i, j) (Number $ digitToInt c) w
    | otherwise = M.insert (i, j) (Symbol c) w

consolidate :: World -> SpanWorld
consolidate = go M.empty (Nothing) . M.toList where
  go w Nothing [] = w
  go w (Just (ix, n, len)) [] = M.insert ix (Number (n, len)) w
  go w Nothing ((ix, Symbol c):rest) =
    go (M.insert ix (Symbol c) w) Nothing rest
  go w (Just ((i, j), n, len)) ((ix, Symbol c):rest) =
    go
      (M.insert (i, j) (Number (n, len)) $ M.insert ix (Symbol c) w)
      Nothing rest
  go w Nothing (((i, j), Number n):rest) = go w (Just ((i, j), n, 1)) rest
  go w (Just ((i, j), n, len)) (((i', j'), Number n'):rest)
    | i' == i && j' == j + len = go w (Just ((i, j), n * 10 + n', len + 1)) rest
    | otherwise =
        go (M.insert (i, j) (Number (n, len)) w) (Just ((i', j'), n', 1)) rest

isSymbol :: Cell a -> Bool
isSymbol (Number _) = False
isSymbol (Symbol _) = True

adjacentSymbol :: SpanWorld -> (Int, Int) -> Int -> Bool
adjacentSymbol w (i, j) len =
  any adjacentSymbol' [(i, j + d) | d <- [0..len - 1]]
  where
    neighbors (x, y) =
      [ (x, y - 1)
      , (x, y + 1)
      , (x - 1, y)
      , (x + 1, y)
      , (x - 1, y - 1)
      , (x + 1, y - 1)
      , (x - 1, y + 1)
      , (x + 1, y + 1)
      ]
    adjacentSymbol' pos = any hasSymbol (neighbors pos)
    hasSymbol pos = case M.lookup pos w of
      Nothing -> False
      Just c -> isSymbol c

partNumbers :: SpanWorld -> [Int]
partNumbers w = go $ M.toList w where
  go [] = []
  go ((_, Symbol _):rest) = go rest
  go ((pos, Number (n, len)):rest)
    | adjacentSymbol w pos len = n:go rest
    | otherwise = go rest

main :: IO ()
main = do
  world <- parse . lines <$> getContents
  {-
  let world' = consolidate world
  print world'
  -}
  print $ sum $ partNumbers $ consolidate world
