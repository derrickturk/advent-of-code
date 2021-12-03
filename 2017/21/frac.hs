{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (guard)
import Data.Maybe (fromJust)
import Data.List (foldl', transpose)
import qualified Data.Map.Strict as M

import FemtoParsec

data Grid
  = Grid { rows :: [[Bool]]
         , size :: Int
         } deriving (Eq, Show, Ord)

type Rewrite = (Grid, Grid)
type RewriteCompiled = M.Map Grid Grid

mirror :: Grid -> Grid
mirror (Grid rs sz) = Grid (reverse <$> rs) sz

-- I'm so lazy
rotate :: Grid -> Grid
rotate (Grid rs sz) = mirror $ Grid (transpose rs) sz

permutations :: Grid -> [Grid]
permutations g =
  [ g
  , rotate g
  , rotate $ rotate g
  , rotate $ rotate $ rotate g
  , mirror g
  , rotate $ mirror g
  , rotate $ rotate $ mirror g
  , rotate $ rotate $ rotate $ mirror g
  ]

compile :: [Rewrite] -> RewriteCompiled
compile = foldl' f M.empty where
  f m (from, to) = foldl' (\m' g -> M.insert g to m') m $ permutations from

startPattern :: Grid
startPattern = fromJust $ parse grid2d
  ".#.\n\
  \..#\n\
  \###\n"

cell :: Parser Bool
cell =  True <$ char '#'
    <|> False <$ char '.'

grid1d :: Parser Grid
grid1d = do
  rs <- sepBy (char '/') $ some cell
  let len = length rs
  guard $ all (== len) $ length <$> rs
  pure $ Grid rs len

grid2d :: Parser Grid
grid2d = do
  rs <- some $ lexeme $ some cell
  let len = length rs
  guard $ all (== len) $ length <$> rs
  pure $ Grid rs len

rewrite :: Parser Rewrite
rewrite = (,) <$> (lexeme grid1d <* lexeme "=>") <*> grid1d

reChonk :: [[Grid]] -> [[Grid]]
reChonk gs =
  let totalSize = sum $ size <$> head gs
      reChonk' n gs = undefined
   in if totalSize `mod` 2 == 0
        then reChonk' 2 gs
        else reChonk' 3 gs

main :: IO ()
main = do
  Just rules <- parseStdin $ some $ lexeme rewrite
  let rules' = compile rules
  print startPattern
  print rules'
