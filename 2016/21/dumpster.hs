{-# LANGUAGE FlexibleContexts, RankNTypes, OverloadedStrings #-}

import Control.Monad.ST
import Data.Array.ST
import Data.Array.IArray (elems)
import Control.Monad (foldM, forM_)
import System.Environment (getArgs)
import Data.Char (isAlpha)
import Data.List (find)
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
  | UnRotLtr Char -- the FORBIDDEN ANTI-INSTRUCTION
  deriving Show

undo :: Instr -> Instr
undo (SwapPos x y) = (SwapPos y x)
undo (SwapLtr c d) = (SwapLtr d c)
undo (RotL n) = RotR n
undo (RotR n) = RotL n
undo (RotLtr c) = UnRotLtr c
undo (RevSpan i j) = RevSpan i j
undo (Move i j) = Move j i
undo (UnRotLtr c) = RotLtr c

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
  (0, end) <- getBounds a
  let len = end + 1
  mapIndices (0, end) (\i -> (i + n) `mod` len) a
run a (RotR n) = do
  (0, end) <- getBounds a
  let len = end + 1
  mapIndices (0, end) (\i -> (i - n) `mod` len) a
run a (RotLtr c) = do
  which <- find ((== c) . snd) <$> getAssocs a
  case which of
    Just (i, _) -> run a (RotR $ 1 + (if i >= 4 then 1 else 0) + i)
    Nothing -> pure a
run a (RevSpan from to) = do
  (0, end) <- getBounds a
  let remap i = if i < from || i > to
                  then i
                  else to - (i - from)
  mapIndices (0, end) remap a
run a (Move i j) = do
  (0, end) <- getBounds a
  val <- readArray a i
  -- "delete" at i
  forM_ [i..end - 1] $ \k -> do
    x <- readArray a (k + 1)
    writeArray a k x
  -- "insert" at j
  forM_ [end, end - 1 .. j + 1] $ \k -> do
    x <- readArray a (k - 1)
    writeArray a k x
  writeArray a j val
  pure a
run a (UnRotLtr c) = do
  a' <- run a (RotL 1)
  go 1 a'
  where
    unrotix i = if i < 6
                  then i - 1
                  else i - 2
    go n arr = do
      (0, end) <- getBounds arr
      let i = unrotix n
      if i >= end
        then pure arr
        else do
          x <- readArray arr i
          if x == c
            then pure arr
            else do
              arr' <- run arr (RotL 1)
              if n == 4
                then do
                  arr'' <- run arr' (RotL 1)
                  go 6 arr''
                else go (n + 1) arr'

scramble :: String -> [Instr] -> String
scramble pass instrs = elems $ runSTUArray $ do
  a <- initialize pass
  foldM run a instrs

unscramble :: String -> [Instr] -> String
unscramble pass instrs = elems $ runSTUArray $ do
  a <- initialize pass
  foldM (\arr i -> run arr (undo i)) a $ reverse instrs

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
  putStr "SCRAMBLE: "
  print $ scramble pass instrs
  putStr "UNSCRAMBLE: "
  print $ unscramble pass instrs
