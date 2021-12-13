{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldl')
import qualified Data.Set as S

import FemtoParsec

type Pos = (Int, Int)
type Sheet = S.Set Pos

data Fold
  = AlongX Int
  | AlongY Int
  deriving Show

applyFold :: Sheet -> Fold -> Sheet
applyFold s (AlongX x)
  = S.map (\(x', y') -> if x' <= x then (x', y') else (x - (x' - x), y')) s
applyFold s (AlongY y)
  = S.map (\(x', y') -> if y' <= y then (x', y') else (x', y - (y' - y))) s

problem :: Parser (Sheet, [Fold])
problem = (,) <$> (S.fromList <$> many (lexeme point))
              <*> many (lexeme instr)
  where
    point = (,) <$> (unsignedIntNum <* ",") <*> unsignedIntNum
    instr =  AlongX <$> ("fold along x=" *> unsignedIntNum)
         <|> AlongY <$> ("fold along y=" *> unsignedIntNum)

dump :: Sheet -> [String]
dump s =
  let s' = S.toList s
      minX = minimum $ fst <$> s'
      maxX = maximum $ fst <$> s'
      minY = minimum $ snd <$> s'
      maxY = maximum $ snd <$> s'
   in [ [if S.member (x, y) s then '#' else ' ' | x <- [minX..maxX]]
        | y <- [minY..maxY]
      ]

main :: IO ()
main = do
  Just (pts, instrs) <- parseStdin problem
  print $ S.size $ applyFold pts $ head instrs
  let folded = foldl' applyFold pts instrs
  mapM_ putStrLn $ dump folded
