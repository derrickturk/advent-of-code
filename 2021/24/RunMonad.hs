{-# LANGUAGE TypeApplications #-}

import Control.Monad (guard)

import Monad
import Interval

import FemtoParsec

intervalize :: CPU Int -> CPU Interval
intervalize (CPU w x y z) =
  CPU (singleton w) (singleton x) (singleton y) (singleton z)

mightZeroZ :: [Instr] -> CPU Interval -> Bool
mightZeroZ is c = let (State c' ()) = run is (State c ())
                   in z c' `contains` 0

zeroingZ :: [Int] -> [Instr] -> [[Int]]
zeroingZ inputs = go (State boot []) where
  go s@(State c _) is =
    case is of
      (Input v):rest -> do
        val <- inputs
        let c' = write val v c
            c'' = intervalize c'
        guard (mightZeroZ rest c'')
        vals <- go (State c' []) rest
        pure $ val:vals
      i:rest -> go (step i s) rest
      [] -> do
        guard (z c == 0)
        pure []

main :: IO ()
main = do
  Just prog <- parseStdin program
  let biggest = take 1 $ zeroingZ [9, 8..1] prog
      smallest = take 1 $ zeroingZ [1..9] prog
  print $ biggest
  print $ smallest
