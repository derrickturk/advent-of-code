{-# LANGUAGE TypeApplications #-}

import Data.List (find)
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

max14 :: Int
max14 = 99999999999999

main :: IO ()
main = do
  Just prog <- parseStdin program
  [num] <- getArgs
  let Just input = readMaybe @Int num
      accepted n = case run @Int @[Int] prog (State boot $ toDigits n) of
        Just (State c []) -> z c == 0
        _ -> False
  print $ accepted input
