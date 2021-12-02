{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.ST
import Data.Array.ST
import Data.Array.IArray (elems)
import Control.Monad (foldM)
import Data.Char (isAlpha)

import FemtoParsec

-- spin = rotR, no it doesn't, yes it does

data Instr
  = SwapPos Int Int
  | SwapLtr Char Char
  | RotR Int
  deriving Show

initialize :: String -> ST s (STUArray s Int Char)
initialize s = newListArray (0, length s - 1) s

run :: STUArray s Int Char -> Instr -> ST s (STUArray s Int Char)
run a (SwapPos i j) = do
  x <- readArray a i
  y <- readArray a j
  writeArray a i y
  writeArray a j x
  pure a
run a (SwapLtr c d) = mapArray swap a where
  swap x
    | x == c = d
    | x == d = c
    | otherwise = x
run a (RotR n) = do
  (0, end) <- getBounds a
  let len = end + 1
  mapIndices (0, end) (\i -> (i - n) `mod` len) a

instr :: Parser Instr
instr = swapPos <|> swapLtr <|> rotR
  where
    swapPos = SwapPos <$> ("x" *> unsignedIntNum)
                      <*> ("/" *> unsignedIntNum)
    swapLtr = SwapLtr <$> ("p" *> letter)
                      <*> ("/" *> letter)
    rotR = RotR <$> ("s" *> unsignedIntNum)
    letter = charP isAlpha

dance :: [Instr] -> String -> String
dance instrs initial = elems $ runSTUArray $ do
  a <- initialize initial
  foldM run a instrs

cycleTime :: [Instr] -> String -> Int
cycleTime instrs initial =
  let chain = iterate (dance instrs) initial
   in 1 + (length $ takeWhile (/= initial) $ tail chain)

main :: IO ()
main = do
  Just instrs <- parseStdin $ lexeme $ sepBy "," instr
  let initial = ['a'..'p']
  putStrLn $ dance instrs initial
  let cyc = cycleTime instrs initial
      toRun = 1000000000 `mod` cyc
  putStrLn $ iterate (dance instrs) initial !! toRun
