{-# LANGUAGE StrictData #-}

import Control.Monad.ST
import Data.STRef
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.List (findIndex)
import Data.Int (Int8)

type Recipes s = MV.MVector s Int8

data GameState s = GameState { scores :: Recipes s
                             , i1 :: STRef s Int
                             , i2 :: STRef s Int
                             }

initial :: ST s (GameState s)
initial = GameState <$> (V.thaw $ V.fromList [3, 7])
                    <*> newSTRef 0
                    <*> newSTRef 1

stepGame :: GameState s -> ST s (GameState s)
stepGame (GameState scores i1 i2) = do
  s1 <- readSTRef i1 >>= MV.read scores
  s2 <- readSTRef i2 >>= MV.read scores
  let combined = s1 + s2
  scores' <- if combined >= 10
    then do
      s' <- MV.grow scores 2
      MV.write s' (MV.length s' - 2) 1
      MV.write s' (MV.length s' - 1) (combined - 10)
      pure s'
    else do
      s' <- MV.grow scores 1
      MV.write s' (MV.length s' - 1) combined
      pure s'
  modifySTRef i1 $ (`mod` MV.length scores') . (+ (1 + fromIntegral s1))
  modifySTRef i2 $ (`mod` MV.length scores') . (+ (1 + fromIntegral s2))
  pure $ GameState scores' i1 i2
{-# INLINE stepGame #-}

runTilLength :: Int -> GameState s -> ST s (GameState s)
runTilLength n s
  | MV.length (scores s) >= n = pure s
  | otherwise = stepGame s >>= runTilLength n

match :: Recipes s -> V.Vector Int8 -> ST s Bool
match = go 0 where
  go n mvec vec
    | n == MV.length mvec = pure True
    | otherwise = do
        x <- MV.read mvec n
        if x == vec V.! n
          then go (n + 1) mvec vec
          else pure False
{-# INLINE match #-}

solvePt1 :: Int -> ST s [Int8]
solvePt1 n = do
  final <- initial >>= runTilLength (n + 10)
  V.toList <$> (V.freeze $ MV.slice n 10 $ scores final)

solvePt2 :: [Int8] -> ST s Int
solvePt2 digits = do
  let needle = V.fromList $ digits
  s <- initial
  go needle s
  where
    go needle s@(GameState mvec _ _)
      | MV.length mvec < V.length needle = stepGame s >>= go needle
      | otherwise = do
          let lm = MV.length mvec
              ln = V.length needle
          if lm - ln > 0
            then do
              m2 <- match (MV.slice (lm - ln - 1) ln mvec) needle
              if m2
                then pure (lm - ln - 1)
                else do
                  m1 <- match (MV.slice (lm - ln) ln mvec) needle
                  if m1
                    then pure (lm - ln)
                    else stepGame s >>= go needle
            else do
              m1 <- match (MV.slice (lm - ln) ln mvec) needle
              if m1
                then pure (lm - ln)
                else stepGame s >>= go needle

main :: IO ()
main = do
  input <- getLine
  putStrLn $ concatMap show $ runST $ solvePt1 (read input)
  print $ runST $ solvePt2 (read . pure <$> input)
