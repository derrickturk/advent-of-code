{-# LANGUAGE StrictData #-}

-- https://www.redblobgames.com/grids/hexagons/#coordinates
-- but rotated 90 degrees clockwise

{-
                 E (+X/-Z) => (1,0,-1)
                          _____
  NE (+Y/-Z) => (0,1,-1) /     \  SE (+X/-Y) => (1,-1,0)
                        /(0,0,0)\
                        \       /
  NW (-X/+Y) => (-1,1,0) \_____/  SW (-Y/+Z) => (0,-1,1)
              
                 W (-X/+Z) => (-1,0,1)
-}

import Data.List (foldl')
import qualified Data.Set as S

newtype HexCoord = HexCoord { getHexCoord :: (Int, Int, Int) }
  deriving (Eq, Ord, Show)

data HexMove = E
             | SE
             | SW
             | W
             | NW
             | NE
             deriving (Eq, Ord, Show)

origin :: HexCoord
origin = HexCoord (0, 0, 0)

move :: HexCoord -> HexMove -> HexCoord
move (HexCoord (x, y, z)) E = HexCoord (x + 1, y, z - 1)
move (HexCoord (x, y, z)) SE = HexCoord (x + 1, y - 1, z)
move (HexCoord (x, y, z)) SW = HexCoord (x, y - 1, z + 1)
move (HexCoord (x, y, z)) W = HexCoord (x - 1, y, z + 1)
move (HexCoord (x, y, z)) NW = HexCoord (x - 1, y + 1, z)
move (HexCoord (x, y, z)) NE = HexCoord (x, y + 1, z - 1)

movePath :: HexCoord -> [HexMove] -> HexCoord
movePath c = foldl' move c

-- just the black tiles
newtype HexGrid = HexGrid { getHexGrid :: S.Set HexCoord }

initialGrid :: HexGrid
initialGrid = HexGrid S.empty

flipTile :: HexGrid -> HexCoord -> HexGrid
flipTile (HexGrid g) c = if S.member c g
  then HexGrid $ S.delete c g
  else HexGrid $ S.insert c g

blackTiles :: HexGrid -> Int
blackTiles = S.size . getHexGrid

flipTiles :: [[HexMove]] -> HexGrid
flipTiles = foldl' flipTile initialGrid . fmap (movePath origin)

parseMove :: String -> Maybe (String, HexMove)
parseMove ('e':rest) = Just (rest, E)
parseMove ('s':'e':rest) = Just (rest, SE)
parseMove ('s':'w':rest) = Just (rest, SW)
parseMove ('w':rest) = Just (rest, W)
parseMove ('n':'w':rest) = Just (rest, NW)
parseMove ('n':'e':rest) = Just (rest, NE)
parseMove _ = Nothing

parseMoves :: String -> [HexMove]
parseMoves input = case parseMove input of
  Just (rest, dir) -> dir:parseMoves rest
  Nothing -> []

main :: IO ()
main = do
  tileSeqs <- fmap parseMoves . lines <$> getContents
  print $ blackTiles $ flipTiles tileSeqs
