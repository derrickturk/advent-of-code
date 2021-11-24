{-# LANGUAGE TupleSections #-}

import Data.List (sort)
import FemtoParsec

triangle :: Parser (Int, Int, Int)
triangle = (,,) <$> lexeme intNum <*> lexeme intNum <*> lexeme intNum

valid :: (Int, Int, Int) -> Bool
valid (x, y, z) = let [x', y', z'] = sort [x, y, z]
                   in x' + y' > z'

rejigger :: [(Int, Int, Int)] -> [(Int, Int, Int)]
rejigger ((a, b, c):(d, e, f):(g, h, i):rest) =
  (a, d, g):(b, e, h):(c, f, i):rejigger rest
rejigger _ = []

main :: IO ()
main = do
  Just tris <- parseStdin (space *> many triangle)
  print $ length $ filter valid tris
  print $ length $ filter valid $ rejigger tris
