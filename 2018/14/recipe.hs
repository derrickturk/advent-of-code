import qualified Data.Vector.Unboxed as V
import Data.List (findIndex)
import Data.Int (Int8)

type Recipes = V.Vector Int8

digits :: (Show a, Read a) => a -> [a]
digits = fmap (read . pure) . show

combine :: Recipes -> Int -> Int -> Recipes
combine vec i j = V.fromList $ digits $ vec V.! i + vec V.! j

update :: Recipes -> Int -> Int -> (Recipes, Int, Int)
update vec i j =
  let vec' = vec <> combine vec i j
      i' = (i + 1 + fromIntegral (vec V.! i)) `mod` V.length vec'
      j' = (j + 1 + fromIntegral (vec V.! j)) `mod` V.length vec'
  in (vec', i', j')

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

runTilLength :: Recipes -> Int -> Int -> Int -> (Recipes, Int, Int)
runTilLength vec i j n
  | V.length vec >= n = (vec, i, j)
  | otherwise = let (vec', i', j') = update vec i j in runTilLength vec' i' j' n

initial :: Recipes
initial = V.fromList [3, 7]

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

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

fourth :: (a, b, c, d) -> d
fourth (_, _, _, x) = x

solvePt1 :: Int -> Recipes
solvePt1 n = V.slice n 10 (fst3 $ runTilLength initial 0 1 (n + 10))

solvePt2 :: Recipes -> Int
solvePt2 = fourth . runTilSubseq initial 0 1

main :: IO ()
main = do
  input <- read <$> getLine
  putStrLn $ concatMap show $ V.toList (solvePt1 input)
  print $ solvePt2 $ V.fromList $ fromIntegral <$> digits input
