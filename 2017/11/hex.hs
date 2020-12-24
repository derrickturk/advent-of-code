{-# LANGUAGE StrictData #-}

-- hell insanity
-- https://www.redblobgames.com/grids/hexagons/#coordinates

{-
                 N (+X/-Z) => (1,0,-1)
                          _____
  NW (+Y/-Z) => (0,1,-1) /     \  NE (+X/-Y) => (1,-1,0)
                        /(0,0,0)\
                        \       /
  SW (-X/+Y) => (-1,1,0) \_____/  SE (-Y/+Z) => (0,-1,1)
              
                 S (-X/+Z) => (-1,0,1)
-}

import Data.List (foldl')

newtype HexCoord = HexCoord { getHexCoord :: (Int, Int, Int) }
  deriving (Eq, Ord, Show)

data HexMove = N
             | NE
             | SE
             | S
             | SW
             | NW
             deriving (Eq, Ord, Show)

origin :: HexCoord
origin = HexCoord (0, 0, 0)

move :: HexCoord -> HexMove -> HexCoord
move (HexCoord (x, y, z)) N = HexCoord (x + 1, y, z - 1)
move (HexCoord (x, y, z)) NE = HexCoord (x + 1, y - 1, z)
move (HexCoord (x, y, z)) SE = HexCoord (x, y - 1, z + 1)
move (HexCoord (x, y, z)) S = HexCoord (x - 1, y, z + 1)
move (HexCoord (x, y, z)) SW = HexCoord (x - 1, y + 1, z)
move (HexCoord (x, y, z)) NW = HexCoord (x, y + 1, z - 1)

movePath :: HexCoord -> [HexMove] -> HexCoord
movePath c = foldl' move c

scanPath :: HexCoord -> [HexMove] -> [HexCoord]
scanPath c = scanl move c

manhattan :: HexCoord -> HexCoord -> Int
manhattan (HexCoord (x0, y0, z0)) (HexCoord (x1, y1, z1)) =
  maximum $ fmap abs [x1 - x0, y1 - y0, z1 - z0]

parseMove :: String -> Maybe HexMove
parseMove "n" = Just N
parseMove "ne" = Just NE
parseMove "se" = Just SE
parseMove "s" = Just S
parseMove "sw" = Just SW
parseMove "nw" = Just NW
parseMove _ = Nothing

split :: Eq a => a -> [a] -> [[a]]
split d xs = case dropWhile (== d) xs of
  [] -> []
  xs' -> let (x, rest) = break (== d) xs'
         in x:split d rest

main :: IO ()
main = do
  input <- traverse parseMove . split ',' <$> getLine
  case input of 
    Nothing -> putStrLn "invalid input"
    Just moves -> do
      print $ manhattan origin (movePath origin moves)
      print $ maximum $ manhattan origin <$> scanPath origin moves
