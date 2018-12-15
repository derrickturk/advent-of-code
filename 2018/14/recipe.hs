import qualified Data.Vector.Unboxed as V
import Data.Int (Int8)

type Recipes = V.Vector Int8

combine :: Recipes -> Int -> Int -> Recipes
combine vec i j = V.fromList $ fmap (read . pure) $ show $ vec V.! i + vec V.! j

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

fst3 :: (a, b, c) -> a
fst3 (x, y, z) = x

solvePt1 :: Int -> Recipes
solvePt1 n = V.slice n 10 (fst3 $ runTilLength initial 0 1 (n + 10))

main :: IO ()
main = do
  input <- read <$> getLine
  putStrLn $ concatMap show $ V.toList (solvePt1 input)
