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
