{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State
import qualified Data.Map.Strict as M

import FemtoParsec

data GameState
  = GameState { players :: M.Map Int (Int, Int)
              , nextPlayer :: [Int]
              , nextRoll :: Int
              , rolls :: Int
              }

beginGame :: [(Int, Int)] -> GameState
beginGame ps =
  let ps' = M.fromList $ (\(n, i) -> (n, (i, 0))) <$> ps
      nextP = cycle $ M.keys ps'
   in GameState ps' nextP 1 0

roll :: State GameState Int
roll = do
  x <- gets nextRoll
  modify (\s -> let r = nextRoll s
                 in s { nextRoll = if r + 1 > 100 then 1 else r + 1 })
  modify (\s -> s { rolls = rolls s + 1 })
  pure x

upNext :: State GameState Int
upNext = do
  plrs <- gets nextPlayer
  case plrs of
    (who:rest) -> do
      modify (\s -> s { nextPlayer = rest })
      pure who
    _ -> error "that shouldn't happen"

play :: State GameState Bool
play = do
  plr <- upNext
  (i, score) <- gets $ (M.! plr) . players
  r1 <- roll
  r2 <- roll
  r3 <- roll
  let n = r1 + r2 + r3
      i' = (i - 1 + n) `mod` 10 + 1
      score' = score + i'
      win = score' >= 1000
  modify (\s -> s { players = M.insert plr (i', score') $ players s })
  pure win

game :: State GameState ()
game = do
  won <- play
  if won
    then pure ()
    else game

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
  let final = execState game (beginGame ps)
      lostScore = head $ filter (< 1000) $ snd <$> M.elems (players final)
      nRolls = rolls final
  print $ lostScore * nRolls
