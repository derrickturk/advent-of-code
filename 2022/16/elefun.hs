{-# LANGUAGE OverloadedRecordDot, OverloadedStrings #-}

import Data.Char (isAlpha)
import qualified Data.Text as T
import qualified Data.Map.Strict as M

{-
import Q
import qualified Data.Set as S
-}

import Dijkstra
import FemtoParsec

data Valve
  = Valve { name :: T.Text
          , rate :: Int
          , tunnels :: [T.Text]
          }
  deriving (Show, Eq, Ord)

valveP :: Parser Valve
valveP = Valve <$> (lexeme "Valve" *> lexeme (chars1 isAlpha))
               <*> ("has flow rate=" *> unsignedIntNum)
               <*> (lexeme (plural "; tunnel") *>
                    lexeme (plural "lead") *>
                    lexeme (plural "to valve") *>
                    sepBy ", " (chars1 isAlpha))
  where
    plural kw = kw <* optional "s"

type World = M.Map T.Text (Int, [T.Text])

worldP :: Parser World
worldP = do
  valves <- some $ lexeme valveP
  pure $ M.fromList $ fmap (\v -> (v.name, (v.rate, v.tunnels))) valves

data State
  = State { open :: M.Map T.Text Int
          , position :: T.Text
          , minute :: Int
          , totalFlow :: Int
          } deriving (Show, Eq, Ord)

initial :: State
initial = State M.empty "AA" 0 0

validMoves :: World -> State -> [State]
validMoves w s = open <> move where
  totalFlow' = s.totalFlow + sum (M.elems s.open) 
  minute' = s.minute + 1
  move = do
    stepTo <- snd $ w M.! s.position
    pure $ s { position = stepTo, totalFlow = totalFlow', minute = minute' }
  open = if M.member s.position s.open
    then []
    else [ s { open = M.insert s.position (fst $ w M.! s.position) s.open
             , totalFlow = totalFlow'
             , minute = minute'
             }
         ]

costlyMoves :: World -> State -> [(Int, State)]
costlyMoves w s = fmap f $ validMoves w s where
  f s' = (s.totalFlow - s'.totalFlow, s')

succs :: World -> State -> [(Int, State)]
succs w s = ss <> concatMap (succs w . snd) ss
  where ss = costlyMoves w s

{-
bruteForce :: World -> State -> [State]
bruteForce w s = go (singleton s) S.empty where
  go Empty _ = []
  go (s' :<| q') seen
    | S.member s' seen = go q' seen
    | s'.minute == 20 = s':go q' seen
    | otherwise = go (append q' $ validMoves w s') (S.insert s' seen)
-}

main :: IO ()
main = do
  Just world <- parseStdin worldP -- fallacy
  -- print world
  -- print $ take 250 $ succs world initial
  print $ costToWin initial (costlyMoves world) (\s -> s.minute == 10)
  -- print $ maximum $ totalFlow <$> bruteForce world initial
