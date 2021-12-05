import qualified Data.Map.Strict as M
import Data.List (foldl')
import Dijkstra

data Entity
  = Goblin Int Int
  | Elf Int Int
  deriving Show

data Feature
  = Wall
  | Floor
  deriving Show

type Pos = (Int, Int)

data World = World { features :: M.Map Pos Feature
                   , entities :: M.Map Pos Entity
                   } deriving Show

parseWorld :: [String] -> Maybe World
parseWorld = fmap (make . concat) . traverse (uncurry parseRow) . zip [0..] where
  parseRow :: Int -> String -> Maybe [(Either Feature Entity, Pos)]
  parseRow i = traverse parseCell . zip [(i, j) | j <- [0..]]

  parseCell :: (Pos, Char) -> Maybe (Either Feature Entity, Pos)
  parseCell (p, '#') = Just (Left Wall, p)
  parseCell (p, '.') = Just (Left Floor, p)
  parseCell (p, 'G') = Just (Right (Goblin 200 3), p)
  parseCell (p, 'E') = Just (Right (Elf 200 3), p)
  parseCell (_, _) = Nothing

  make :: [(Either Feature Entity, Pos)] -> World
  make = uncurry World . foldl' makeStep (M.empty, M.empty)

  makeStep :: (M.Map Pos Feature, M.Map Pos Entity)
           -> (Either Feature Entity, Pos)
           -> (M.Map Pos Feature, M.Map Pos Entity)
  makeStep (fs, es) (Left f, p) = (M.insert p f fs, es)
  {- a tricky invariant here: all entities occupy Floor;
   - if they die or move away we need to know about it
   - so we'll always check for Entity first, then Feature
   -}
  makeStep (fs, es) (Right e, p) = (M.insert p Floor fs, M.insert p e es)

main :: IO ()
main = do
  Just world <- parseWorld . lines <$> getContents
  print world
