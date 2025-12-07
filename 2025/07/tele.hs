import Control.Monad (foldM)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

data Manifold = Manifold Int (Int, Int) (S.Set (Int, Int)) deriving Show

data Move
  = Continue (Int, Int)
  | Split (Int, Int) (Int, Int)
  | Fizzle
  deriving Show

enumerate2d :: [[a]] -> [((Int, Int), a)]
enumerate2d rows = [ ((i, j), x)
                   | (i, row) <- zip [0..] rows
                   , (j, x) <- zip [0..] row
                   ]

parse :: [String] -> Maybe Manifold
parse rows = do
  (maxI, origins, splitters) <-
    foldM accum (0, S.empty, S.empty) (enumerate2d rows)
  case S.elems origins of
    [o] -> pure $ Manifold maxI o splitters
    _ -> Nothing
  where
    accum (maxI, origins, splitters) ((i, _), '.') =
      pure (max i maxI, origins, splitters)
    accum (maxI, origins, splitters) (ix@(i, _), 'S') =
      pure (max i maxI, S.insert ix origins, splitters)
    accum (maxI, origins, splitters) (ix@(i, _), '^') =
      pure (max i maxI, origins, S.insert ix splitters)
    accum _ _ = Nothing

moves :: Manifold -> S.Set (Int, Int) -> [Move]
moves (Manifold maxI _ splitters) = fmap move . S.toList where
  move (i, j)
    | i + 1 > maxI = Fizzle
    | (i + 1, j) `S.member` splitters = Split (i + 1, j + 1) (i + 1, j - 1)
    | otherwise = Continue (i + 1, j)

newPositions :: [Move] -> S.Set (Int, Int)
newPositions = S.fromList . concatMap f where
  f Fizzle = []
  f (Continue ix) = [ix]
  f (Split ix1 ix2) = [ix1, ix2]

countSplits :: [Move] -> Int
countSplits = go 0 where
  go n [] = n
  go n (Fizzle:rest) = go n rest
  go n (Continue _:rest) = go n rest
  go n (Split _ _:rest) = go (n + 1) rest

start :: Manifold -> S.Set (Int, Int)
start (Manifold _ o _) = S.singleton o

runCountingSplits :: Manifold -> Int
runCountingSplits m =
  case dropWhile (not . S.null . snd) $ iterate f (0, start m) of
    ((n, _):_) -> n
    _ -> error "not possible"
  where
    f (n, beams) = let ms = moves m beams
                    in (n + countSplits ms, newPositions ms)

runCountingTimelines :: Manifold -> Int
runCountingTimelines m@(Manifold _ o _) = go 0 (M.fromList [(o, 1)]) where
  go n beams
    | M.null beams = n
    | otherwise =
        go'
          n
          M.empty
          (zip (M.toList beams) (moves m $ S.fromList $ M.keys beams))
  go' n beams [] = go n beams
  go' n beams (((_, tls), Fizzle):rest) = go' (n + tls) beams rest
  go' n beams (((_, tls), Continue ix):rest) =
    go' n (M.insertWith (+) ix tls beams) rest
  go' n beams (((_, tls), Split ix1 ix2):rest) =
    go' n (M.insertWith (+) ix1 tls $ M.insertWith (+) ix2 tls beams) rest

main :: IO ()
main = do
  Just mani <- parse . lines <$> getContents
  print $ runCountingSplits mani
  print $ runCountingTimelines mani
