import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (foldl')
import Data.Tuple (swap)
import Prelude hiding (map)

import Dijkstra

data Cell
  = Open
  | Goal Char
  deriving (Eq, Show, Ord)

type Map = M.Map (Int, Int) Cell

parseMap :: [String] -> Map
parseMap = foldl' parseRow M.empty . zip [0..] where
  parseRow map (y, row) = foldl' (parseCell y) map $ zip [0..] row
  parseCell y map (x, '.') = M.insert (x, y) Open map
  parseCell _ map (_, '#') = map
  parseCell y map (x, g) = M.insert (x, y) (Goal g) map

allGoals :: Map -> S.Set Char
allGoals map = S.fromList $ [g | Goal g <- M.elems map]

data State = State { pos :: (Int, Int)
                   , goals :: S.Set Char
                   } deriving (Eq, Show, Ord)

validSteps :: Map -> State -> [State]
validSteps map s@(State (x, y) gs) = do
  pos' <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  case M.lookup pos' map of
    Nothing -> []
    Just Open -> pure $ s { pos = pos' }
    Just (Goal g) -> pure $ State pos' (S.insert g gs)

won :: S.Set Char -> State -> Bool
won gs = (== gs) . goals

won2 :: S.Set Char -> (Int, Int) -> State -> Bool
won2 gs target (State p visited) = p == target && gs == visited

initialState :: Map -> Maybe State
initialState map = do
  startPos <- lookup (Goal '0') $ fmap swap $ M.toList map
  pure $ State startPos (S.singleton '0')

main :: IO ()
main = do
  map <- parseMap . lines <$> getContents
  let Just initial = initialState map
  print $
    costToWin initial (countingSteps $ validSteps map) (won $ allGoals map)
  print $ costToWin initial
    (countingSteps $ validSteps map) (won2 (allGoals map) (pos initial))
