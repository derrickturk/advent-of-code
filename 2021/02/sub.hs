{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldl')
import FemtoParsec

data Dir
  = Up
  | Down
  | Forward
  deriving Show

type Cmd = (Dir, Int)

type Pos = (Int, Int)

move :: Pos -> Cmd -> Pos
move (x, y) (Up, n) = (x, y - n)
move (x, y) (Down, n) = (x, y + n)
move (x, y) (Forward, n) = (x + n, y)

dir :: Parser Dir
dir =  Up <$ "up"
   <|> Down <$ "down"
   <|> Forward <$ "forward"

cmd :: Parser Cmd
cmd = (,) <$> lexeme dir <*> unsignedIntNum

main :: IO ()
main = do
  Just cmds <- parseStdin $ many $ lexeme cmd
  let (x, y) = foldl' move (0, 0) cmds
  print $ x * y
