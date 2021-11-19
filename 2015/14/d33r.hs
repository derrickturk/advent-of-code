{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isSpace)
import Text.Read (readMaybe)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Foldable (traverse_)
import Control.Monad.State (State, evalState, gets, modify)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M

type Deer = (T.Text, Int, Int, Int)

parseDeer :: T.Text -> Maybe Deer
parseDeer line = case T.split isSpace line of
  [ name
    , "can"
    , "fly"
    , speed
    , "km/s"
    , "for"
    , duration
    , "seconds,"
    , "but"
    , "then"
    , "must"
    , "rest"
    , "for"
    , rest
    , "seconds."
    ] -> (,,,) <$> pure name
               <*> readMaybe (T.unpack speed)
               <*> readMaybe (T.unpack duration)
               <*> readMaybe (T.unpack rest)
  _ -> Nothing

deerPos :: Int -> Deer -> Int
deerPos time (_, speed, duration, rest) =
  let perCycle = speed * duration
      cycleWithRest = duration + rest
      fullCycles = time `div` cycleWithRest
      partialCycle = time `rem` cycleWithRest
      partialCycle' = partialCycle `min` duration
   in fullCycles * perCycle + partialCycle' * speed

type DeerContest = M.Map T.Text Int

deerRace :: Int -> [Deer] -> (T.Text, Int)
deerRace time deer = evalState deerRace' initial where
  deerRace' = do
    traverse_ (deerStep deer) [1..time]
    gets (maximumBy (comparing snd) . M.toList)
  initial = M.fromList $ (\(name, _, _, _) -> (name, 0)) <$> deer

deerStep :: [Deer] -> Int -> State DeerContest ()
deerStep deer time = do
  let positions = deerPos time <$> deer
      positions' = zip ((\(name, _, _, _) -> name) <$> deer) positions
      fastest = maximum positions
      winners = filter ((== fastest) . snd) positions'
  traverse_ (\(name, _) -> modify (M.adjust (+ 1) name)) winners

main :: IO ()
main = do
  Just deer <- traverse parseDeer . T.lines <$> TIO.getContents
  let positions = deerPos 2503 <$> deer
  print $ maximum positions
  print $ deerRace 2503 deer
