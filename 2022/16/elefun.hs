{-# LANGUAGE PatternSynonyms, OverloadedRecordDot, OverloadedStrings #-}

import Data.Char (isAlpha)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Q (pattern Empty, pattern (:<|))
import qualified Q

import Beefs
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
          } deriving (Show, Eq, Ord)

initial :: State
initial = State M.empty "AA" 0

-- flowy guys only
type World2 = M.Map T.Text (Int, [(Int, T.Text)])

compile :: World -> World2
compile w =
  let flowy = M.filter (\(f, _) -> f > 0) w
      flowyKeys = M.keys flowy
      froms = "AA":flowyKeys
      tos = flowyKeys
      cost from to = fromJust $ seekSteps from (\n -> snd $ w M.! n) (== to)
      allCosts from = cost from <$> tos
      rec f = (f, (fst $ w M.! f, zip (allCosts f) tos))
   in M.fromList $ rec <$> froms

validMoves :: World -> State -> [(Int, State)]
validMoves w s = (flow, ) <$> open <> move where
  flow = sum (M.elems s.open)
  minute' = s.minute + 1
  move = do
    stepTo <- snd $ w M.! s.position
    pure $ s { position = stepTo, minute = minute' }
  open = if M.member s.position s.open || fst (w M.! s.position) == 0
    then []
    else [ s { open = M.insert s.position (fst $ w M.! s.position) s.open
             , minute = minute'
             }
         ]

validMoves2 :: World2 -> State -> [(Int, Int, State)]
validMoves2 _ s
  | s.minute >= 30 = []
validMoves2 w s = open <> move where
  flow = sum (M.elems s.open)
  move = do
    (minutes, stepTo) <- snd $ w M.! s.position
    pure $
      ( flow * minutes
      , minutes
      , s { position = stepTo, minute = s.minute + minutes }
      )
  open = if M.member s.position s.open
    then []
    else [ ( flow
           , 1
           , s { open = M.insert s.position (fst $ w M.! s.position) s.open
               , minute = s.minute + 1
               }
           )
         ]

costlyMoves :: World -> Int -> State -> [(Int, State)]
costlyMoves w maxTotal s = (\(c, s') -> (maxTotal - c, s')) <$> validMoves w s

costlyMoves2 :: World2 -> Int -> State -> [(Int, State)]
costlyMoves2 w maxTotal s =
  (\(c, m, s') -> (maxTotal * m - c, s')) <$> validMoves2 w s

-- doesn't seem to help trim things at all
totalAddressable :: World -> T.Text -> Int
totalAddressable w n = go (Q.singleton n) S.empty 0 where
  go Empty _ f = f
  go (n' :<| q') seen f
    | S.member n' seen = go q' seen f
    | otherwise =
        let (f', ns) = w M.! n'
         in go (Q.append q' ns) (S.insert n' seen) (f + f')

main :: IO ()
main = do
  Just world <- parseStdin worldP -- fallacy
  -- print world
  let maxTotal = sum $ fst <$> M.elems world
  -- print $ compile world
  print $ (maxTotal * 30) - costToWin initial (costlyMoves2 (compile world) maxTotal) (\s -> s.minute == 30)
  -- print $ totalAddressable world <$> M.keys world
