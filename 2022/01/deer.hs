{-# LANGUAGE TypeApplications #-}

import Data.List (sortBy, unfoldr)
import Data.Ord (comparing, Down(..))
import Text.Read (readMaybe)

breakOn :: Eq a => a -> [a] -> ([a], [a])
breakOn e = go [] where
  go prefixRev [] = (reverse prefixRev, [])
  go prefixRev l@(x:xs)
    | x == e = (reverse prefixRev, l)
    | otherwise = go (x:prefixRev) xs

{- a little goofy in that it drops the trailing delimiter #-}
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn e es = unfoldr f es where
  f xs = case breakOn e xs of
    ([], []) -> Nothing
    (pre, []) -> Just (pre, [])
    (pre, _:post) -> Just (pre, post)

main :: IO ()
main = do
  elves <- splitOn "" . lines <$> getContents
  let calories = fmap sum <$> traverse (traverse (readMaybe @Int)) elves
  case sortBy (comparing Down) <$> calories of
    Just (one:two:three:_) -> do
      print one
      print $ one + two + three
    _ -> putStrLn "bad input"
