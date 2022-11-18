{-# LANGUAGE TypeApplications #-}

import System.Environment (getArgs)
import Text.Read (readMaybe)

import Monad

import FemtoParsec

toDigits :: Int -> [Int]
toDigits = pad14 . fmap fromChar . show where
  fromChar c = fromEnum c - fromEnum '0' 
  pad14 ds
    | length ds >= 14 = ds
    | otherwise = replicate (14 - length ds) 0 <> ds

main :: IO ()
main = do
  Just prog <- parseStdin program
  [num] <- getArgs
  Just input <- pure (readMaybe @Int num)
  let final n = run @Int @[Int] prog (State boot $ toDigits n)
  print $ final input
