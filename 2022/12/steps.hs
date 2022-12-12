import Control.Monad (guard)
import qualified Data.Map.Strict as M

import Dijkstra (costToWin, countingSteps)

type World = M.Map (Int, Int) Char

parse :: String -> World
parse txt = M.fromList [((i, j), c)
  | (j, l) <- zip [0..] (lines txt), (i, c) <- zip [0..] l]

start :: World -> (Int, Int)
start = fst . head . filter ((== 'S') . snd) . M.toList

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (i, j) = [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]

validMoves :: World -> (Int, Int) -> [(Int, Int)]
validMoves w (i, j) = do
  let this = case w M.! (i, j) of
        'S' -> 'a'
        c -> c
  (i', j') <- neighbors (i, j)
  that <- case M.lookup (i', j') w of
    Just 'E' -> pure 'z'
    Just c -> pure c
    Nothing -> []
  guard $ (fromEnum that - fromEnum this) <= 1
  pure (i', j')

main :: IO ()
main = do
  world <- parse <$> getContents
  let s = start world
      ss = fmap fst $ filter ((\c -> c == 'S' || c == 'a') . snd) $
        M.toList world
      win p = world M.! p == 'E'
  print $ costToWin s (countingSteps $ validMoves world) win
  print $ minimum $
    fmap (\s' -> costToWin s' (countingSteps $ validMoves world) win) ss
