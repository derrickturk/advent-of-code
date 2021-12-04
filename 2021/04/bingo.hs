{-# LANGUAGE OverloadedStrings, TupleSections #-}

import Control.Monad (replicateM)
import Data.List (partition, transpose)

import FemtoParsec

type Draws = [Int]

newtype Board = Board { getBoard :: [[(Int, Bool)]] }
  deriving Show

draw :: Int -> Board -> Board
draw n = Board . fmap (fmap f) . getBoard where
  f (x, _)
    | x == n = (x, True)
  f (x, seen) = (x, seen)

won :: Board -> Bool
won (Board rows) = any (all snd) rows || any (all snd) (transpose rows)

score :: Int -> Board -> Int
score winning (Board rows) =
  winning * (sum $ fmap fst $ filter (not . snd) $ concat rows)

draws :: Parser Draws
draws = sepBy "," unsignedIntNum

board :: Parser Board
board = Board <$> replicateM 5
  (replicateM 5 ((,) <$> lexeme unsignedIntNum <*> pure False))

winners :: Draws -> [Board] -> [(Int, Board)]
winners [] _ = []
winners (d:ds) bs = let (ws, ls) = partition won $ draw d <$> bs
                     in ((d,) <$> ws) <> winners ds ls

main :: IO ()
main = do
  Just (ds, bs) <- parseStdin $ (,) <$> lexeme draws <*> some board
  let ws = winners ds bs
  print $ uncurry score $ head ws
  print $ uncurry score $ last ws
