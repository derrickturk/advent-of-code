import qualified Data.Text as T

import FemtoParsec
import Dijkstra

data Machine
  = Machine { spec :: [Bool]
            , buttons :: [[Int]]
            , reqs :: [Int]
            } deriving Show

initMachine :: Machine -> [Bool]
initMachine = (False <$) . spec

movesMachine :: Machine -> [Bool] -> [(Int, [Bool])]
movesMachine m lights = (1,) . toggle lights <$> buttons m where
  toggle = go 0
  go i (b:restB) flips@(j:restF)
    | i == j = not b:go (i + 1) restB restF
    | otherwise = b:go (i + 1) restB flips
  go _ bs [] = bs
  go _ [] _ = []

solveMachine :: Machine -> Int
solveMachine m = costToWin (initMachine m) (movesMachine m) (== spec m)

machineP :: Parser Machine
machineP = Machine <$> specP <*> buttonsP <*> reqsP where
  specP = do
    _ <- char '['
    lights <- chars1 (\c -> c == '.' || c == '#')
    _ <- lexeme $ char ']'
    pure $ (== '#') <$> T.unpack lights
  buttonsP = some $
    char '(' *> sepBy (char ',') unsignedIntNum <* lexeme (char ')')
  reqsP = do
    _ <- char '{'
    reqs <- sepBy (char ',') unsignedIntNum
    _ <- lexeme $ char '}'
    pure reqs

main :: IO ()
main = do
  Just machines <- parseStdin (some machineP)
  print $ sum $ solveMachine <$> machines
