{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (guard)
import Data.Maybe (listToMaybe)
import qualified Data.Set as S

import Combo
import FemtoParsec

type Scanner = (Int, [(Int, Int, Int)])

type Rotation = ((Int, Int, Int), (Int, Int, Int), (Int, Int, Int))

rotate :: Rotation -> (Int, Int, Int) -> (Int, Int, Int)
rotate ((a0, a1, a2), (b0, b1, b2), (c0, c1, c2)) (x, y, z)
  = ( a0 * x + a1 * y + a2 * z
    , b0 * x + b1 * y + b2 * z
    , c0 * x + c1 * y + c2 * z
    )

rotationsOf :: [(Int, Int, Int)] -> [[(Int, Int, Int)]]
rotationsOf pts = (\r -> rotate r <$> pts) <$> rotations

alignTo :: [(Int, Int, Int)] -> (Int, Int, Int) -> S.Set (Int, Int, Int)
alignTo xs (x, y, z) = S.fromList $
  (\(x', y', z') -> (x' - x, y' - y, z' - z)) <$> xs

realign :: S.Set (Int, Int, Int) -> (Int, Int, Int) -> S.Set (Int, Int, Int)
realign s (x, y, z) = S.map (\(x', y', z') -> (x' - x, y' - y, z' - z)) s

unify :: S.Set (Int, Int, Int)
      -> S.Set (Int, Int, Int)
      -> [[(Int, Int, Int)]]
      -> Maybe (S.Set (Int, Int, Int), S.Set (Int, Int, Int), [[(Int, Int, Int)]])
unify bcns scnrs coords = listToMaybe $ do
  (cs, rest) <- leaveOneOut coords
  rotated <- rotationsOf cs

  curOriginBcn <- S.toList bcns
  let bcns' = realign bcns curOriginBcn

  matchOriginBcn <- rotated
  let rotated' = alignTo rotated matchOriginBcn

  guard $ S.size (S.intersection bcns' rotated') >= 12

  let scnr = diff curOriginBcn matchOriginBcn
  pure (realign (S.union bcns' rotated') (neg curOriginBcn), S.insert scnr scnrs, rest)

unifyAll :: [[(Int, Int, Int)]]
         -> Maybe (S.Set (Int, Int, Int), S.Set (Int, Int, Int))
unifyAll [] = Just (S.empty, S.empty)
unifyAll (coords:rest) = go (S.fromList coords) (S.singleton (0, 0, 0)) rest where
  go bcns scnrs coords' = case unify bcns scnrs coords' of
    Just (bcns', scnrs', []) -> Just (bcns', scnrs')
    Just (bcns', scnrs', coords'') -> go bcns' scnrs' coords''
    Nothing -> Nothing

scanner :: Parser Scanner
scanner = do
  _ <- "--- scanner "
  n <- unsignedIntNum
  _ <- lexeme " ---"
  coords <- some $ lexeme $
    (,,) <$> (intNum <* ",") <*> (intNum <* ",") <*> intNum
  pure (n, coords)

manhattan :: (Int, Int, Int) -> (Int, Int, Int) -> Int
manhattan (x0, y0, z0) (x1, y1, z1) =
  abs (x0 - x1) + abs (y0 - y1) + abs (z0 - z1)

neg :: (Int, Int, Int) -> (Int, Int, Int)
neg (x, y, z) = (-x, -y, -z)

diff :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
diff (x0, y0, z0) (x1, y1, z1) = (x1 - x0, y1 - y0, z1 - z0)

main :: IO ()
main = do
  Just scanners <- parseStdin $ some scanner
  let Just (beacons, scanners') = (unifyAll $ snd <$> scanners)
  print $ S.size beacons
  print $ maximum $ do
    let scanners'' = S.toList scanners'
    s1 <- scanners''
    s2 <- scanners''
    pure $ manhattan s1 s2

rotations :: [Rotation]
rotations = 
  [
  ( (1, 0, 0)
  , (0, 1, 0)
  , (0, 0, 1)
  )
  ,
  ( (1, 0, 0)
  , (0, 0, -1)
  , (0, 1, 0)
  )
  ,
  ( (1, 0, 0)
  , (0, -1, 0)
  , (0, 0, -1)
  )
  ,
  ( (1, 0, 0)
  , (0, 0, 1)
  , (0, -1, 0)
  )
  ,
  ( (0, -1, 0)
  , (1, 0, 0)
  , (0, 0, 1)
  )
  ,
  ( (0, 0, 1)
  , (1, 0, 0)
  , (0, 1, 0)
  )
  ,
  ( (0, 1, 0)
  , (1, 0, 0)
  , (0, 0, -1)
  )
  ,
  ( (0, 0, -1)
  , (1, 0, 0)
  , (0, -1, 0)
  )
  ,
  ( (-1, 0, 0)
  , (0, -1, 0)
  , (0, 0, 1)
  )
  ,
  ( (-1, 0, 0)
  , (0, 0, -1)
  , (0, -1, 0)
  )
  ,
  ( (-1, 0, 0)
  , (0, 1, 0)
  , (0, 0, -1)
  )
  ,
  ( (-1, 0, 0)
  , (0, 0, 1)
  , (0, 1, 0)
  )
  ,
  ( (0, 1, 0)
  , (-1, 0, 0)
  , (0, 0, 1)
  )
  ,
  ( (0, 0, 1)
  , (-1, 0, 0)
  , (0, -1, 0)
  )
  ,
  ( (0, -1, 0)
  , (-1, 0, 0)
  , (0, 0, -1)
  )
  ,
  ( (0, 0, -1)
  , (-1, 0, 0)
  , (0, 1, 0)
  )
  ,
  ( (0, 0, -1)
  , (0, 1, 0)
  , (1, 0, 0)
  )
  ,
  ( (0, 1, 0)
  , (0, 0, 1)
  , (1, 0, 0)
  )
  ,
  ( (0, 0, 1)
  , (0, -1, 0)
  , (1, 0, 0)
  )
  ,
  ( (0, -1, 0)
  , (0, 0, -1)
  , (1, 0, 0)
  )
  ,
  ( (0, 0, -1)
  , (0, -1, 0)
  , (-1, 0, 0)
  )
  ,
  ( (0, -1, 0)
  , (0, 0, 1)
  , (-1, 0, 0)
  )
  ,
  ( (0, 0, 1)
  , (0, 1, 0)
  , (-1, 0, 0)
  )
  ,
  ( (0, 1, 0)
  , (0, 0, -1)
  , (-1, 0, 0)
  )
  ]
