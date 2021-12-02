{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldl')
import FemtoParsec

data Dir
  = Up
  | Down
  | Forward
  deriving Show

type Cmd = (Dir, Int)

type Pos = (Int, Int, Int)

move :: Pos -> Cmd -> Pos
move (x, y, aim) (Up, n) = (x, y, aim - n)
move (x, y, aim) (Down, n) = (x, y, aim + n)
move (x, y, aim) (Forward, n) = (x + n, y + aim * n, aim)

dir :: Parser Dir
dir =  Up <$ "up"
   <|> Down <$ "down"
   <|> Forward <$ "forward"

cmd :: Parser Cmd
cmd = (,) <$> lexeme dir <*> unsignedIntNum

main :: IO ()
main = do
  Just cmds <- parseStdin $ many $ lexeme cmd
  let (x, y, _) = foldl' move (0, 0, 0) cmds
  print $ x * y
