{-# LANGUAGE TypeApplications #-}

import Control.Monad (guard)
import Data.List (nub)

presents :: Integer -> Integer
presents n = (* 10) . sum $ do
  k <- [1..floor (sqrt @Double (fromInteger n))]
  guard (n `mod` k == 0)
  [k, n `div` k]

presents2 :: Integer -> Integer
presents2 n = (* 11) . sum . nub $ do
  k <- [1..50]
  guard (n `mod` k == 0)
  [k, n `div` k]

main :: IO ()
main = do
  print $ fst $ head $ dropWhile ((<= 34000000) . snd) $
    zip [1::Int ..] $ presents <$> [1..]
  print $ fst $ head $ dropWhile ((<= 34000000) . snd) $
    zip [1::Int ..] $ presents2 <$> [1..]
