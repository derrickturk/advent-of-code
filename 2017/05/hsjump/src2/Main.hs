module Main where

import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Text.Read (readMaybe)

makeMemory :: [Int] -> ST s (MV.MVector s Int)
makeMemory = V.thaw . V.fromList

runInstruction :: (MV.MVector s Int) -> Int -> ST s Int
runInstruction vec ip = do
  jump <- MV.read vec ip
  MV.modify vec (\x -> if x >= 3 then x - 1 else x + 1) ip
  pure $ ip + jump

stepsToHalt :: [Int] -> ST s Int
stepsToHalt insts = do
  mem <- makeMemory insts
  go mem 0 0
  where
  go mem count ip = do
    if ip < 0 || ip >= MV.length mem
      then pure count
      else runInstruction mem ip >>= go mem (count + 1)

parseMemory :: String -> Maybe [Int]
parseMemory = traverse readMaybe . words

main :: IO ()
main = do
  input <- parseMemory <$> getContents
  case input of
    Nothing -> putStrLn "invalid input"
    Just mem -> print $ runST $ stepsToHalt mem
