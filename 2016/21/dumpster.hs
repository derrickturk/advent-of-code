{-# LANGUAGE RankNTypes, OverloadedStrings #-}

import Control.Monad.ST
import Data.Array.ST
import Data.Array.IArray (elems)
import Control.Monad (foldM)
import System.Environment (getArgs)
import Data.Char (isAlpha)
import Prelude hiding (min, max)

import FemtoParsec

data Instr
  = SwapPos Int Int
  | SwapLtr Char Char
  | RotL Int
  | RotR Int
  | RotLtr Char
  | RevSpan Int Int
  | Move Int Int
  deriving Show

initialize :: String -> ST s (STUArray s Int Char)
initialize pass = newListArray (0, length pass - 1) pass

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
run a (RotL n) = do
  (min, max) <- getBounds a
  let len = max - min + 1
  newElems <- traverse
    (\i -> readArray a (min + ((i - min + n) `mod` len))) [min..max]
  newListArray (min, max) newElems
run a (RotR n) = do
  (min, max) <- getBounds a
  let len = max - min + 1
  newElems <- traverse
    (\i -> readArray a (min + ((i - min - n) `mod` len))) [min..max]
  newListArray (min, max) newElems
run a _ = pure a

scramble :: String -> [Instr] -> String
scramble pass instrs = elems $ runSTUArray $ do
  a <- initialize pass
  foldM run a instrs

instr :: Parser Instr
instr = swapPos <|> swapLtr <|> rotL <|> rotR <|> rotLtr <|> revSpan <|> move
  where
    swapPos = SwapPos <$> ("swap position " *> unsignedIntNum)
                      <*> (" with position " *> unsignedIntNum)
    swapLtr = SwapLtr <$> ("swap letter " *> letter)
                      <*> (" with letter " *> letter)
    rotL = RotL <$> ("rotate left " *> unsignedIntNum <* (" step" >> optional "s"))
    rotR = RotR <$> ("rotate right " *> unsignedIntNum <* (" step" >> optional "s"))
    rotLtr = RotLtr <$> ("rotate based on position of letter " *> letter)
    revSpan = RevSpan <$> ("reverse positions " *> unsignedIntNum)
                      <*> (" through " *> unsignedIntNum)
    move = Move <$> ("move position " *> unsignedIntNum)
                <*> (" to position " *> unsignedIntNum)
    letter = charP isAlpha

main :: IO ()
main = do
  [pass] <- getArgs
  Just instrs <- parseStdin $ many $ lexeme instr
  print instrs
  print $ scramble pass instrs
