{-# LANGUAGE OverloadedStrings #-}

import Data.List (foldl')
import qualified Data.Map.Strict as M

import FemtoParsec

data GameState
  = GameState { players :: M.Map Int (Int, Int)
              , nextPlayer :: [Int]
              }

instance Show GameState where
  show (GameState plrs _) = "GameState " <> show plrs <> " (and some cycles)"

beginGame :: [(Int, Int)] -> GameState
beginGame ps =
  let ps' = M.fromList $ (\(n, i) -> (n, (i, 0))) <$> ps
      nextP = cycle $ M.keys ps'
   in GameState ps' nextP

play :: GameState -> [(GameState, Maybe Int)]
play (GameState plrs (who:rest)) = do
  let (i, score) = plrs M.! who
  r1 <- [1, 2, 3]
  r2 <- [1, 2, 3]
  r3 <- [1, 2, 3]
  let n = r1 + r2 + r3
      i' = (i - 1 + n) `mod` 10 + 1
      score' = score + i'
      win = score' >= 21
      plrs' = M.insert who (i', score') plrs
  pure (GameState plrs' rest, if win then Just who else Nothing)
play _ = error "that shouldn't happen"

gameWinners :: GameState -> [Int]
gameWinners s = do
  (s', won) <- play s
  case won of
    Just i -> pure i
    Nothing -> gameWinners s'

countWins :: [Int] -> M.Map Int Int
countWins is = foldl' f M.empty is where
  f m i = M.insertWith (+) i 1 m

playerPos :: Parser (Int, Int)
playerPos = do
  _ <- "Player "
  n <- unsignedIntNum
  _ <- " starting position: "
  i <- unsignedIntNum
  pure (n, i)

main :: IO ()
main = do
  Just ps <- parseStdin $ some $ lexeme playerPos
  {-
  let game = beginGame ps
      game' = play game
      stillGoing = filter ((== Nothing) . snd) game'
  print stillGoing
  -}
  let counts = countWins $ gameWinners $ beginGame ps
  print $ maximum $ M.elems counts
