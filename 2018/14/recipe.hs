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

digits :: (Show a, Read a) => a -> [a]
digits = fmap (read . pure) . show

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

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

{-
subseqIndex :: Recipes -> Recipes -> Maybe Int
subseqIndex vec sub
  | V.length sub > V.length vec = Nothing
  | otherwise = (+ fromIx) <$> findIndex (subseq vec sub) [fromIx..toIx] where
      fromIx = max (V.length vec - V.length sub - 2) 0
      toIx = V.length vec - V.length sub
      subseq vec sub i = V.slice i (V.length sub) vec == sub

runTilSubseq :: Recipes
             -> Int
             -> Int
             -> Recipes
             -> (Recipes, Int, Int, Int)
runTilSubseq vec i j sub = case subseqIndex vec sub of
  Just n -> (vec, i, j, n)
  _ -> let (vec', i', j') = update vec i j in runTilSubseq vec' i' j' sub
-}

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

fourth :: (a, b, c, d) -> d
fourth (_, _, _, x) = x

solvePt1 :: Int -> ST s [Int8]
solvePt1 n = do
  final <- initial >>= runTilLength (n + 10)
  V.toList <$> (V.freeze $ MV.slice n 10 $ scores final)

{-
solvePt2 :: Recipes -> Int
solvePt2 = fourth . runTilSubseq initial 0 1
-}

main :: IO ()
main = do
  input <- read <$> getLine
  putStrLn $ concatMap show $ runST $ solvePt1 input
  -- print $ solvePt2 $ V.fromList $ fromIntegral <$> digits input
