{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (guard)
import Data.Maybe (fromJust)
import Data.List (foldl', transpose)
import qualified Data.Map.Strict as M

import ChunkyWindows
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

rewrite :: Parser Rewrite
rewrite = (,) <$> (lexeme grid1d <* lexeme "=>") <*> grid1d

dissolve :: [[Grid]] -> [[Bool]]
dissolve = concat . fmap dissolveRow where
  dissolveRow :: [Grid] -> [[Bool]]
  dissolveRow = fmap concat . transpose . fmap rows

reChonk :: [[Grid]] -> [[Grid]]
reChonk gs =
  let totalSize = sum $ size <$> head gs
   in if totalSize `mod` 2 == 0
        then meld 2 $ dissolve gs
        else meld 3 $ dissolve gs

meld :: Int -> [[Bool]] -> [[Grid]]
meld n = fmap (meldRow n) . chunks' n where
  meldRow m = fmap (\g -> Grid g m) . transpose . fmap (chunks' m)

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

game :: RewriteCompiled -> [[[Grid]]]
game rules = iterate (reChonk . runRewrites rules) [[startPattern]]

runRewrites :: RewriteCompiled -> [[Grid]] -> [[Grid]]
runRewrites rules = (fmap . fmap) (rules M.!)

countOn :: Grid -> Int
countOn = length . filter id . concat . rows

countOn' :: [[Grid]] -> Int
countOn' = sum . fmap countOn . concat

main :: IO ()
main = do
  Just rules <- parseStdin $ some $ lexeme rewrite
  let rules' = compile rules
      steps = game rules'
  print $ countOn' $ steps !! 2
  print $ countOn' $ steps !! 5
  print $ countOn' $ steps !! 18
