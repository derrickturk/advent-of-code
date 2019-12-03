{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State.Strict
import Text.Read (readMaybe)
import System.IO (stderr)
import Data.List (elemIndex, sortBy)
import Data.Maybe (fromJust, listToMaybe)
import System.Environment (getArgs)

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Dir = R | L | U | D deriving (Eq, Show)

type Wire = [(Dir, Int)]

newtype Loc = Loc { getLoc :: (Int, Int) }
  deriving (Eq, Show)

manhattanFromOrigin :: Loc -> Int
manhattanFromOrigin (Loc (x, y)) = abs x + abs y
{-# INLINE manhattanFromOrigin #-}

-- ordered by manhattan distance from the origin, then x, then y
instance Ord Loc where
  compare l1@(Loc (x1, y1)) l2@(Loc (x2, y2)) =
    case compare (manhattanFromOrigin l1) (manhattanFromOrigin l2) of
      EQ -> case compare x1 x2 of
        EQ -> compare y1 y2
        neq -> neq
      neq -> neq
  {-# INLINE compare #-}

path :: Wire -> [Loc]
path = fmap Loc . concat . (`evalState` (0, 0)) . traverse (state . step) where
  step (R, n) (x, y) = ([(x + i, y) | i <- [1..n]], (x + n, y))
  step (L, n) (x, y) = ([(x - i, y) | i <- [1..n]], (x - n, y))
  step (U, n) (x, y) = ([(x, y + j) | j <- [1..n]], (x, y + n))
  step (D, n) (x, y) = ([(x, y - j) | j <- [1..n]], (x, y - n))
{-# INLINE path #-}

intersect :: Wire -> Wire -> S.Set Loc
intersect w1 w2 = S.fromList (path w1) `S.intersection` S.fromList (path w2)
{-# INLINE intersect #-}

shortestSteps :: Wire -> Wire -> Maybe (Loc, Int, Int)
shortestSteps w1 w2 =
  let path1 = path w1
      path2 = path w2
      int = S.toList $ intersect w1 w2
      steps seq pt = fromJust (elemIndex pt seq) + 1
      cmpSteps pt1 pt2 = compare
        (steps path1 pt1 + steps path2 pt1)
        (steps path1 pt2 + steps path2 pt2)
      mkResult pt = (pt, steps path1 pt, steps path2 pt) in
      mkResult <$> (listToMaybe $ sortBy cmpSteps int)

parseWire :: T.Text -> Maybe Wire
parseWire = traverse parseStep . T.splitOn "," where
  parseStep step
    | T.null step = Nothing
    | otherwise = (,) <$> parseDir (T.head step)
                      <*> readMaybe (T.unpack $ T.tail step)
  parseDir 'R' = Just R
  parseDir 'L' = Just L
  parseDir 'U' = Just U
  parseDir 'D' = Just D
  parseDir _ = Nothing

problem1 :: Wire -> Wire -> IO ()
problem1 w1 w2 = case S.lookupMin (intersect w1 w2) of
  Just loc -> print $ manhattanFromOrigin loc
  _ -> TIO.hPutStrLn stderr "No intersections."

problem2 :: Wire -> Wire -> IO ()
problem2 w1 w2 = case shortestSteps w1 w2 of
  Just (loc, steps1, steps2) -> print $ steps1 + steps2
  _ -> TIO.hPutStrLn stderr "No intersections."

main :: IO ()
main = do
  args <- getArgs
  let action = case args of
        ["1"] -> Just problem1
        ["2"] -> Just problem2
        _ -> Nothing
  case action of
    Just m -> do
      wires <- traverse parseWire . T.lines <$> TIO.getContents
      case wires of
        Just [w1, w2] -> m w1 w2
        _ -> TIO.hPutStrLn stderr "Invalid input."
    Nothing -> TIO.hPutStrLn stderr "Usage: cross 1|2 <input.txt"
