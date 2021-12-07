import qualified Data.Map.Strict as M
import Control.Monad (foldM, guard)
import Data.List (find, foldl', sort)
import Data.Maybe (isNothing, listToMaybe, maybeToList)
import Prelude hiding (id, round)

import AllShortest

data EntityKind
  = Goblin
  | Elf
  deriving (Eq, Show)

-- ok, it's really just their starting pos
newtype EID = EID { getEID :: (Int, Int) }
  deriving (Eq, Show)

newtype Dmg = Dmg { getDmg :: Int }
  deriving (Eq, Show)

newtype HP = HP { getHP :: Int }
  deriving (Eq, Show)

data Entity = Entity EntityKind EID Dmg HP
  deriving Show

data Feature
  = Wall
  | Floor
  deriving Show

type Pos = (Int, Int)

data World = World { features :: M.Map Pos Feature
                   , entities :: M.Map Pos Entity
                   } deriving Show

adjacent :: Pos -> [Pos]
adjacent (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

openAdjacent :: World -> Pos -> [Pos]
openAdjacent w p = do
  a <- adjacent p
  guard $ open w a
  pure a

enemy :: EntityKind -> EntityKind
enemy Elf = Goblin
enemy Goblin = Elf

open :: World -> Pos -> Bool
open w p = case entities w M.!? p of
  Just _ -> False
  Nothing -> case features w M.!? p of
    Just Floor -> True
    _ -> False

kindExtinct :: EntityKind -> World -> Bool
kindExtinct k w = isNothing $
  find (\(Entity k' _ _ _) -> k == k') (M.elems $ entities w)

kindAtPos :: EntityKind -> World -> Pos -> Bool
kindAtPos k w p = case entities w M.!? p of
  Just (Entity k' _ _ _) -> k == k'
  Nothing -> False

moveTowardNearest :: World -> Pos -> EntityKind -> Maybe Pos
moveTowardNearest w p k = fmap snd $ listToMaybe $ sort $ do
  (targetPos, _) <- filter (\(_, Entity k' _ _ _) -> k == k') $
    M.toList $ entities w
  targetAdj <- openAdjacent w targetPos
  guard $ open w targetAdj
  let lbls = labelPaths p (openAdjacent w) (== targetAdj)
  depth <- maybeToList $ lbls M.!? targetAdj
  (next:_) <- allShortest' lbls p (openAdjacent w) (== targetAdj)
  pure (depth, next)

-- Left = battle over, Right = continue
round :: World -> Either World World
round w = foldM turn w (M.toList $ entities w)

turn :: World -> (Pos, Entity) -> Either World World
turn w (p, e@(Entity k id dmg hp))
  | kindExtinct (enemy k) w = Left w
  | otherwise = Right $
      let w' = if any (kindAtPos (enemy k) w) (adjacent p)
                 then case moveTowardNearest w p (enemy k) of
                   Just p' ->
                     w { entities = M.insert p' e $ M.delete p $ entities w }
                   Nothing -> w
                  else w
       in w' -- TODO!

game :: World -> (Int, World)
game = go 0 where
  go n w = case round w of
    Left final -> (n, final)
    Right w' -> go (n + 1) w'

parseWorld :: [String] -> Maybe World
parseWorld = fmap (make . concat) . traverse (uncurry parseRow) . zip [0..] where
  parseRow :: Int -> String -> Maybe [(Either Feature Entity, Pos)]
  parseRow i = traverse parseCell . zip [(i, j) | j <- [0..]]

  parseCell :: (Pos, Char) -> Maybe (Either Feature Entity, Pos)
  parseCell (p, '#') = Just (Left Wall, p)
  parseCell (p, '.') = Just (Left Floor, p)
  parseCell (p, 'G') = Just (Right (Entity Goblin (EID p) (Dmg 3) (HP 200)), p)
  parseCell (p, 'E') = Just (Right (Entity Elf (EID p) (Dmg 3) (HP 200)), p)
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
  let (n, world') = game world
  print $ n * sum ((\(Entity _ _ _ (HP h)) -> h) <$> M.elems (entities world'))
  print world'