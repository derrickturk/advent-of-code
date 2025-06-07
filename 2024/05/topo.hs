import qualified Data.Map.Strict as M
import qualified Data.Graph as G
import Data.List (partition)
import Text.Read (readMaybe)

type AdjacencyMap = M.Map Int [Int]
type Edge = (Int, Int)
type Book = [Int]

split :: Char -> String -> [String]
split _ "" = []
split c s =
  let (first, rest) = break (== c) s
      rest' = case rest of
        (_:xs) -> xs
        [] -> []
   in first:split c rest'

parseEdges :: [String] -> Maybe ([Edge], [String])
parseEdges edgeLines =
  let (edges, rest) = break (== "") edgeLines
      parseEdge line = case split '|' line of
        [x, y] -> (,) <$> readMaybe x <*> readMaybe y
        _ -> Nothing
   in (,) <$> (traverse parseEdge edges) <*> pure rest

parseBooks :: [String] -> Maybe [Book]
parseBooks ("":rest) = traverse parseBook rest where
  parseBook line = traverse readMaybe $ split ',' line
parseBooks _ = Nothing

toAdjacencyMap :: [(Int, Int)] -> AdjacencyMap
toAdjacencyMap = foldl' f M.empty where
  f m (x, y) = M.insertWith (<>) x [y] m

fromAdjacencyMap
  :: AdjacencyMap
  -> (G.Graph, G.Vertex -> ((), Int, [Int]), Int -> Maybe G.Vertex)
fromAdjacencyMap m =
  let noded = (\(k, v) -> ((), k, v)) <$> M.toList m
   in G.graphFromEdges noded

parseSpec :: String -> Maybe (M.Map Int [Int], [[Int]])
parseSpec spec = do
  let specLines = lines spec
  (edges, rest) <- parseEdges specLines
  books <- parseBooks rest
  pure (toAdjacencyMap edges, books)

sortBook :: AdjacencyMap -> Book -> Book
sortBook m b =
  let m' = M.filterWithKey (\k _ -> k `elem` b) m
      -- some fiddling to make sure we include "pages" with no "rules"
      m'' = M.union m' $ M.fromList $ zip b $ repeat []
      (g, vn, _) = fromAdjacencyMap m''
      snd3 (_, x, _) = x
   in (snd3 . vn) <$> G.topSort g

middle :: [a] -> a -- gross, unsafe
middle xs = xs !! (length xs `div` 2)

main :: IO ()
main = do
  Just (adjMap, books) <- parseSpec <$> getContents
  let sorted = sortBook adjMap <$> books
  let (good, bad) = partition (uncurry (==)) $ zip books sorted 
  print $ sum $ middle . snd <$> good
  print $ sum $ middle . snd <$> bad
