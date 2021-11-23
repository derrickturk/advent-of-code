{-# LANGUAGE OverloadedStrings #-}

import FemtoParsec

import qualified Data.Set as S

data Direction = L | R | Straight
  deriving (Show, Eq)

data Instr = Instr Direction Int
  deriving (Show, Eq)

data Facing = N | S | E | W
  deriving (Show, Eq)

data State = State Int Int Facing
  deriving (Show, Eq)

origin :: State
origin = State 0 0 N

manhattan :: State -> Int
manhattan (State x y _) = abs x + abs y

turn :: Direction -> Facing -> Facing
turn L N = W
turn L W = S
turn L S = E
turn L E = N
turn R N = E
turn R E = S
turn R S = W
turn R W = N
turn Straight f = f

move :: Int -> State -> State
move n (State x y N) = State x (y + n) N
move n (State x y S) = State x (y - n) S
move n (State x y E) = State (x + n) y E
move n (State x y W) = State (x - n) y W

step :: State -> Instr -> State
step (State x y f) (Instr d n) = move n $ State x y $ turn d f 

direction :: Parser Direction
direction = (L <$ "L") <|> (R <$ "R")

instr :: Parser Instr
instr = Instr <$> direction <*> (fromInteger <$> integer)

prog :: Parser [Instr]
prog = sepBy (lexeme ",") instr

dup :: Ord a => [a] -> Maybe a
dup = go S.empty where
  go _ [] = Nothing
  go seen (x:xs)
    | S.member x seen = Just x
    | otherwise = go (S.insert x seen) xs

shitify :: Instr -> [Instr]
shitify (Instr d n) = (Instr d 1):replicate (n - 1) (Instr Straight 1)

main :: IO ()
main = do
  Just p <- parseStdin $ lexeme prog
  print $ manhattan $ foldl step origin p
  let p' = concatMap shitify p
      coords = (\(State i j _) -> (i, j)) <$> scanl step origin p'
  case dup coords of
    Just (x, y) -> print $ abs x + abs y
    Nothing -> putStrLn "well this is awkward"
