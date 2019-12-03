{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State.Strict
import Text.Read (readMaybe)
import System.IO (stderr)
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

intersect :: Wire -> Wire -> S.Set Loc
intersect w1 w2 = S.fromList (path w1) `S.intersection` S.fromList (path w2)

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

main :: IO ()
main = do
  wires <- traverse parseWire . T.lines <$> TIO.getContents
  case wires of
    Just [w1, w2] -> case S.lookupMin (intersect w1 w2) of
      Just loc -> print $ manhattanFromOrigin loc
      _ -> TIO.hPutStrLn stderr "No intersections."
    _ -> TIO.hPutStrLn stderr "Invalid input."
