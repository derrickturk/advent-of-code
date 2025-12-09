import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.Read (readMaybe)

type Coords = (Int, Int)
type Rect = (Coords, Coords)

newtype RedTiles = RedTiles { getRedTiles :: M.Map Int (S.Set Int) }
  deriving Show

empty :: RedTiles
empty = RedTiles M.empty

insert :: RedTiles -> Coords -> RedTiles
insert (RedTiles m) (x, y) = RedTiles (M.alter f y m) where
  f Nothing = Just $ S.singleton x
  f (Just s) = Just $ S.insert x s

toCoords :: RedTiles -> [Coords]
toCoords (RedTiles m) = [(x, y) | (y, s) <- M.toAscList m, x <- S.elems s]

fromCoords :: [Coords] -> RedTiles
fromCoords = foldl' insert empty

redRects :: RedTiles -> [Rect]
redRects r@(RedTiles m) = concatMap f $ toCoords r where
  f (x, y) = ((x, y),) <$> case M.splitLookup y m of
    (_, Just s, higherY) ->
      [(x', y) | x' <- S.elems $ snd $ S.split x s] <>
      [(x', y') | (y', s') <- M.toAscList higherY, x' <- S.elems s']
    (_, _, _) -> error "invariant broken - map doesn't contain row of element"

area :: Rect -> Int
area ((x1, y1), (x2, y2)) = (abs (x2 - x1) + 1) * (y2 - y1 + 1)

parse :: String -> Maybe Coords
parse s = case break (== ',') s of
  (x, ',':y) -> (,) <$> readMaybe x <*> readMaybe y
  _ -> Nothing

main :: IO ()
main = do
  Just coords <- traverse parse . lines <$> getContents
  let tiles = fromCoords coords
  print $ maximum $ area <$> redRects tiles
