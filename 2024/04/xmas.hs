import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Xmas
  = X
  | M
  | A
  | S
  deriving (Eq, Show)

type Direction = (Int -> Int, Int -> Int)
type WordSearch = M.Map (Int, Int) Xmas
type AIndex = S.Set (Int, Int)
type XIndex = S.Set (Int, Int)

directions :: [Direction]
directions =
  [ (id, (+ 1)) -- right
  , (id, (subtract 1)) -- left
  , ((+ 1), id) -- up
  , ((subtract 1), id) -- down
  , ((+ 1), (+ 1)) -- UR
  , ((+ 1), (subtract 1)) -- UL
  , ((subtract 1), (+ 1)) -- DR
  , ((subtract 1), (subtract 1)) -- DL
  ]

xCoords :: (Int, Int) -> [(Int, Int)]
xCoords (i, j) =
  [ (i, j)
  , (i - 1, j - 1)
  , (i + 1, j + 1)
  , (i - 1, j + 1)
  , (i + 1, j - 1)
  ]

xmasChar :: Char -> Maybe Xmas
xmasChar 'X' = Just X
xmasChar 'M' = Just M
xmasChar 'A' = Just A
xmasChar 'S' = Just S
xmasChar _ = Nothing

indexChars :: String -> [(Int, Int, Char)]
indexChars s = [
    (i, j, c)
    | (i, line) <- zip [0..] (lines s)
    , (j, c) <- zip [0..] line
  ]

parse :: String -> (WordSearch, XIndex, AIndex)
parse = foldl' f (M.empty, S.empty, S.empty) . indexChars where
  f (m, xs, as) (i, j, c) = case xmasChar c of
    Nothing -> (m, xs, as)
    Just X -> (M.insert (i, j) X m, S.insert (i, j) xs, as)
    Just A -> (M.insert (i, j) A m, xs, S.insert (i, j) as)
    Just z -> (M.insert (i, j) z m, xs, as)

isXmas :: WordSearch -> (Int, Int) -> Direction -> Bool
isXmas w ix (fi, fj) =
  let coords = iterate (\(i, j) -> (fi i, fj j)) ix
      found = take 4 $ (w M.!?) <$> coords
   in case found of
        [Just X, Just M, Just A, Just S] -> True
        _ -> False

isXmas' :: WordSearch -> (Int, Int) -> Bool
isXmas' w ix = case (w M.!?) <$> xCoords ix of
  [Just A, Just M, Just S, Just M, Just S] -> True
  [Just A, Just S, Just M, Just M, Just S] -> True
  [Just A, Just M, Just S, Just S, Just M] -> True
  [Just A, Just S, Just M, Just S, Just M] -> True
  _ -> False

countXmas :: WordSearch -> XIndex -> Int
countXmas w xi = length $
  [ () | ix <- S.toList xi, d <- directions, isXmas w ix d ]

countXmas' :: WordSearch -> AIndex -> Int
countXmas' w ai = length $ filter (isXmas' w) $ S.toList ai

main :: IO ()
main = do
  (w, xi, ai) <- parse <$> getContents
  print $ countXmas w xi
  print $ countXmas' w ai
