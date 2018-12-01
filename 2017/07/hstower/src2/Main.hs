module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M
import Data.Monoid (First(..))
import Data.List (foldl', sort, group, sortOn)
import Data.Either (partitionEithers)
import ProgramInfo
import Program

import Debug.Trace

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq (x:xs) = all (== x) xs

oddRest :: Ord a => [a] -> Maybe (a, a)
oddRest xs = case group (sort xs) of
  [[x], (y:_:_)] -> Just (x, y)
  [(y:_:_), [x]] -> Just (x, y)
  _ -> Nothing

modal :: Ord a => [a] -> a
modal = head . head . sortOn (negate . length) . group . sort

balanced :: Program -> Bool
balanced (Program _ _ cs) = allEq (weight <$> cs)

imbalanced' :: Int -> Program -> Maybe (T.Text, Int)
imbalanced' _ (Program _ _ []) = Nothing
imbalanced' t p@(Program n w cs)
  | weight p == t = Nothing
  | allEq (weight <$> cs) = Just (n, w + (t - weight p))
  | otherwise = getFirst $
      foldMap (First . imbalanced' (modal $ weight <$> cs)) cs

imbalanced :: Program -> Maybe (T.Text, Int)
imbalanced (Program _ _ []) = Nothing
imbalanced (Program _ _ cs) = getFirst $
  foldMap (First . imbalanced' (modal $ weight <$> cs)) cs

main :: IO ()
main = do
  progs <- T.lines <$> TIO.getContents
  case partitionEithers ((parse parseProgramInfo "stdin") <$> progs) of
    ([], infos) -> case toRootProgram infos >>= imbalanced of
      Just t -> print t
      Nothing -> putStrLn "no or multiple roots, or no unique imbalanced node"
    (errs, _) -> mapM_ (putStr . parseErrorPretty) errs
