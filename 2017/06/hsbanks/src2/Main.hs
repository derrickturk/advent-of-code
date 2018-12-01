module Main where

import Control.Monad.ST
import Data.STRef
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Text.Read (readMaybe)

data ProgramState s = ProgramState { memory :: MV.MVector s Int
                                   , seen :: STRef s (S.Set (V.Vector Int))
                                   }

makeMemory :: [Int] -> ST s (MV.MVector s Int)
makeMemory = V.thaw . V.fromList

initProgramState :: [Int] -> ST s (ProgramState s)
initProgramState mem = ProgramState <$> makeMemory mem <*> newSTRef (S.empty)

memoryState :: ProgramState s -> ST s (V.Vector Int)
memoryState = V.freeze . memory

stateSeen :: ProgramState s -> V.Vector Int -> ST s Bool
stateSeen st s = S.member s <$> readSTRef (seen st)

runStep :: ProgramState s -> ST s Bool
runStep st = do
  which <- V.maxIndex <$> memoryState st
  distribute (memory st) which
  s <- memoryState st
  seenState <- stateSeen st s
  modifySTRef (seen st) (S.insert s)
  pure seenState

distribute :: MV.MVector s Int -> Int -> ST s ()
distribute mem from = do
  amt <- MV.read mem from
  MV.write mem from 0
  distribute' mem (from + 1) amt
  where
    distribute' _ _ 0 = pure ()
    distribute' vec i n = do
      MV.modify vec (+ 1) (i `mod` MV.length mem)
      distribute' vec (i + 1) (n - 1)

parseMemory :: String -> Maybe [Int]
parseMemory = traverse readMaybe . words

runToSeen :: ProgramState s -> ST s (V.Vector Int)
runToSeen st = do
  done <- runStep st
  if done
    then memoryState st
    else runToSeen st

seenThenLoop :: [Int] -> ST s Int
seenThenLoop mem = do
  st <- initProgramState mem
  s <- runToSeen st
  go 1 st s
  where
    go n st s = do
      _ <- runStep st
      s' <- memoryState st
      if s == s'
        then pure n
        else go (n + 1) st s

main :: IO ()
main = do
  input <- parseMemory <$> getContents
  case input of
    Nothing -> putStrLn "invalid input"
    Just mem -> print $ runST $ seenThenLoop mem 
