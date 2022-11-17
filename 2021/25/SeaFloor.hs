{-# LANGUAGE OverloadedRecordDot #-}

module SeaFloor 
  ( Pos
  , SeaFloor
  , width
  , height
  , easties
  , southies
  , empty
  , fromLines
  , occupied
  , step
  , pprint
  ) where

import Data.List (foldl')
import qualified Data.Set as S

type Pos = (Int, Int)

data SeaFloor
  = SeaFloor { width :: Int
             , height :: Int
             , easties :: S.Set Pos
             , southies :: S.Set Pos
             } deriving (Eq, Show)

empty :: SeaFloor
empty = SeaFloor 0 0 S.empty S.empty

fromLines :: [String] -> SeaFloor
fromLines = fromLines' 0 empty where
  fromLines' _ f [] = f
  fromLines' j f (row:rest) = fromLines' (j + 1) (withRow j row f) rest
  withRow j row f = foldl' (withElem j) f $ zip [0..] row
  withElem j f (i, '>') =
    f { width = i + 1, height = j + 1, easties = S.insert (i, j) f.easties }
  withElem j f (i, 'v') =
    f { width = i + 1, height = j + 1, southies = S.insert (i, j) f.southies }
  withElem j f (i, _) = f { width = i + 1, height = j + 1 }

occupied :: Pos -> SeaFloor -> Bool
occupied p f = S.member p f.easties || S.member p f.southies

step :: SeaFloor -> SeaFloor
step = stepSouthies . stepEasties

pprint :: SeaFloor -> String
pprint f = unlines $
  [[renderPos (i, j) | i <- [0..(f.width - 1)]] | j <- [0..(f.height - 1)]]
  where renderPos p = if S.member p f.easties
                        then '>'
                        else if S.member p f.southies
                          then 'v'
                          else '.'

stepEasties :: SeaFloor -> SeaFloor
stepEasties f = f { easties = S.map step1E f.easties } where
  step1E (i, j) = let i' = (i + 1) `mod` f.width
                   in if (i', j) `occupied` f
                        then (i, j)
                        else (i', j)

stepSouthies :: SeaFloor -> SeaFloor
stepSouthies f = f { southies = S.map step1S f.southies } where
  step1S (i, j) = let j' = (j + 1) `mod` f.height
                   in if (i, j') `occupied` f
                        then (i, j)
                        else (i, j')
