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

statesToSeen :: [Int] -> ST s Int
statesToSeen mem = do
  st <- initProgramState mem
  go 1 st
  where
    go n st = do
      seenState <- runStep st
      if seenState
        then pure n
        else go (n + 1) st

main :: IO ()
main = do
  input <- parseMemory <$> getContents
  case input of
    Nothing -> putStrLn "invalid input"
    Just mem -> print $ runST $ statesToSeen mem 
