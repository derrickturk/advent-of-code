import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Prelude hiding (span)
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

newtype GreenSpan = GreenSpan { getGreenSpan :: [(Int, Int)] }
  deriving Show

emptyGS :: GreenSpan
emptyGS = GreenSpan []

singletonGS :: Int -> Int -> GreenSpan
singletonGS a b = GreenSpan [(a, b)]

insertGS :: GreenSpan -> Int -> Int -> GreenSpan
insertGS (GreenSpan gs) a0 b0 = GreenSpan (insert' a0 b0 gs) where
  insert' a b [] = [(a, b)]
  insert' a b ((a', b'):rest) = case (a < a', b < a', a <= b') of
    (True, True, _) -> (a, b):(a', b'):rest
    (True, False, _) -> insert' a (max b b') rest
    (False, _, True) -> insert' a' (max b b') rest
    (False, _, False) -> (a', b'):insert' a b rest

containingGS :: GreenSpan -> Int -> Maybe (Int, Int)
containingGS (GreenSpan gs) x = find (\(a, b) -> a <= x && x <= b) gs

newtype GreenTiles = GreenTiles { getGreenTiles :: M.Map Int GreenSpan }
  deriving Show

emptyGT :: GreenTiles
emptyGT = GreenTiles M.empty

validRect :: GreenTiles -> Rect -> Bool
validRect (GreenTiles m) ((x1, y1), (x2, y2)) = all validRow [y1..y2] where
  validRow y = case m M.!? y of
    Nothing -> False
    Just gs -> case containingGS gs (min x1 x2) of
      Just (_, x) | x >= max x1 x2 -> True
      _ -> False

paintGreenTiles :: [Coords] -> GreenTiles
paintGreenTiles = paintPoly . foldl' paintSpan emptyGT . loopyPairs where
  loopyPairs [] = []
  loopyPairs xs@(x:_) = loopyPairs' x xs
  loopyPairs' _ [] = []
  loopyPairs' first [x] = [(x, first)]
  loopyPairs' first (x:rest@(y:_)) = (x, y):loopyPairs' first rest
  paintSpan (GreenTiles gt) ((x1, y1), (x2, y2))
    | y1 == y2 = let (minX, maxX) = (min x1 x2, max x1 x2)
                     f (Just span) = Just $ insertGS span minX maxX
                     f Nothing = Just $ singletonGS minX maxX
                  in GreenTiles (M.alter f y1 gt)
    | x1 == x2 = let f (Just span) = Just $ insertGS span x1 x1
                     f Nothing = Just $ singletonGS x1 x1
                     eachRow m y = M.alter f y m
                  in GreenTiles $ foldl' eachRow gt [min y1 y2..max y1 y2]
    | otherwise = GreenTiles gt -- this shouldn't happen!
  paintPoly = GreenTiles
            . M.map (GreenSpan . paintPolyRow . getGreenSpan)
            . getGreenTiles
  paintPolyRow ((a1, _):(_, b2):rest) = (a1, b2):paintPolyRow rest
  paintPolyRow span = span

main :: IO ()
main = do
  Just coords <- traverse parse . lines <$> getContents
  let tiles = fromCoords coords
  print $ maximum $ area <$> redRects tiles
  let greenTiles = paintGreenTiles coords
  print $ maximum $ fmap area $ filter (validRect greenTiles) $ redRects tiles
