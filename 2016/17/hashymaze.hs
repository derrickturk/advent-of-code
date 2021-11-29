import qualified Data.ByteString.Char8 as BS8
import Control.Monad (guard)
import Distribution.Utils.MD5 (md5)
import System.Environment (getArgs)

import Dijkstra (statesToWin, countingSteps)

type Pos = (Int, Int)

data Move
  = U | D | L | R
  deriving (Show, Eq, Ord)

move :: Move -> Pos -> Pos
move U (x, y) = (x, y - 1)
move D (x, y) = (x, y + 1)
move L (x, y) = (x - 1, y)
move R (x, y) = (x + 1, y)

moveChar :: Move -> Char
moveChar U = 'U'
moveChar D = 'D'
moveChar L = 'L'
moveChar R = 'R'

showPath :: [Move] -> String
showPath = fmap moveChar

pathHash :: String -> [Move] -> String
pathHash pass = show . md5 . (BS8.pack pass <>) . BS8.pack . fmap moveChar

hashMoves :: String -> [Move]
hashMoves = fmap fst . filter ((`elem` "bcdef") . snd)
  . zip [U, D, L, R] . take 4

data State = State { pos :: Pos
                   , path :: [Move]
                   } deriving (Show, Eq, Ord)

validMoves :: String -> State -> [State]
validMoves pass s = do
  dir <- hashMoves $ pathHash pass $ path s
  let (x, y) = move dir $ pos s
      path' = path s <> [dir]
  guard $ x >= 0 && x <= 3 && y >= 0 && y <= 3
  pure $ State (x, y) path'

maximaybe :: Ord a => [a] -> Maybe a
maximaybe [] = Nothing
maximaybe xs = Just $ maximum xs

longestPath :: String -> State -> (Int, Int) -> Maybe Int
longestPath pass start finish
  | pos start == finish = Just 0
  | otherwise = maximaybe $ do
      next <- validMoves pass start
      case longestPath pass next finish of
        Just n -> pure $ 1 + n
        Nothing -> []

main :: IO ()
main = do
  [pass] <- getArgs
  let start = State (0, 0) []
      won = (== (3, 3)) . pos
  putStrLn $ showPath $ path $ last $ snd $
    statesToWin start (countingSteps $ validMoves pass) won
  print $ longestPath pass start (3, 3)
