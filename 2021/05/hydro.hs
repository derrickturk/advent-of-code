{-# LANGUAGE OverloadedStrings #-}

-- let's be literal

import qualified Data.Map.Strict as M
import Data.List (foldl')

import FemtoParsec

type Pos = (Int, Int)

data Seg
  = Horiz Int [Int]
  | Vert Int [Int]
  | Oblique [Pos]
  deriving Show

pos :: Parser Pos
pos = (,) <$> (unsignedIntNum <* char ',') <*> unsignedIntNum

seg :: Parser Seg
seg = do
  (x0, y0) <- pos
  _ <- " -> "
  (x1, y1) <- pos
  let x0' = min x0 x1
      x1' = max x0 x1
      y0' = min y0 y1
      y1' = max y0 y1
  if x0 == x1
    then pure $ Vert x0 [y0'..y1']
    else if y0 == y1
      then pure $ Horiz y0 [x0'..x1']
      else if (x0 < x1) == (y0 < y1)
        then pure $ Oblique $ zip [x0'..x1'] [y0'..y1']
        else pure $ Oblique $ zip [x0'..x1'] $ reverse [y0'..y1']

ventCount :: [Seg] -> M.Map Pos Int
ventCount = foldl' f M.empty where
  f m (Horiz y xs) = foldl' (\m' x -> M.insertWith (+) (x, y) 1 m') m xs
  f m (Vert x ys) = foldl' (\m' y -> M.insertWith (+) (x, y) 1 m') m ys
  f m (Oblique _) = m

ventCount2 :: [Seg] -> M.Map Pos Int
ventCount2 = foldl' f M.empty where
  f m (Horiz y xs) = foldl' (\m' x -> M.insertWith (+) (x, y) 1 m') m xs
  f m (Vert x ys) = foldl' (\m' y -> M.insertWith (+) (x, y) 1 m') m ys
  f m (Oblique ps) = foldl' (\m' p -> M.insertWith (+) p 1 m') m ps

main :: IO ()
main = do
  Just segs <- parseStdin $ some $ lexeme seg
  print $ length $ filter ((>= 2) . snd) $ M.toList $ ventCount segs
  print $ length $ filter ((>= 2) . snd) $ M.toList $ ventCount2 segs
