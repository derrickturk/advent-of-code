{-# LANGUAGE TypeApplications #-}

import Data.Array.Unboxed
import Control.Monad.State
import Data.List (sort)
import Text.Read (readMaybe)
import qualified Data.Map.Strict as M

-- they define this with an implicit starting 0
diff :: Num a => [a] -> [a]
diff xs = zipWith (-) xs (0:xs)

adapterChain :: (Ord a, Num a) => [a] -> [a]
adapterChain xs = sort $ (3 + maximum xs):xs

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

chains :: Int
       -> Int
       -> Int
       -> UArray Int Int
       -> State (M.Map (Int, Int) Integer) Integer
chains target last ix xs = do
  val <- gets $ M.lookup (last, ix)
  case val of
    Just count -> pure count
    Nothing -> do
      let (_, ixLast) = bounds xs
      count <- if ix > ixLast
                 then if target == last
                   then pure 1
                   else pure 0
                 else let x = xs ! ix
                       in if x - last > 3
                         then pure 0
                         else (+) <$> chains target x (ix + 1) xs
                                  <*> chains target last (ix + 1) xs
      modify (M.insert (last, ix) count)
      pure count

fullChains :: [Int] -> Integer
fullChains [] = 0
fullChains xs = let lastIx = length xs - 1
                    xs' = listArray (0, lastIx) xs
                 in evalState (chains (xs' ! lastIx) 0 0 xs') M.empty

main :: IO ()
main = do
  inputs <- traverse (readMaybe @Int) . lines <$> getContents
  case inputs of
    Just xs -> do
      let adapters = adapterChain xs
          diffs = diff adapters
          ones = count (== 1) diffs
          threes = count (== 3) diffs
      print $ ones * threes
      print $ fullChains adapters
    _ -> putStrLn "invalid input"
