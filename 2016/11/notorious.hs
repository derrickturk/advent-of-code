{-# LANGUAGE OverloadedStrings #-}

-- the notorious RTG

import qualified Data.Text as T
import qualified Data.Set as S
import Control.Monad (guard)
import Data.List (foldl')

import FemtoParsec

data Floor
 = One
 | Two
 | Three
 | Four
 deriving (Show, Eq, Ord)

data Item
  = Chip T.Text
  | RTG T.Text
  deriving (Show, Eq, Ord)

data State
  = State { elevatorFloor :: Floor
          , firstFloor :: S.Set Item
          , secondFloor :: S.Set Item
          , thirdFloor :: S.Set Item
          , fourthFloor :: S.Set Item
          } deriving (Show, Eq)

floorItems :: Floor -> State -> S.Set Item
floorItems One = firstFloor
floorItems Two = secondFloor
floorItems Three = thirdFloor
floorItems Four = fourthFloor

withFloor :: Floor -> S.Set Item -> State -> State
withFloor One contents s = s { firstFloor = contents }
withFloor Two contents s = s { secondFloor = contents }
withFloor Three contents s = s { thirdFloor = contents }
withFloor Four contents s = s { fourthFloor = contents }

validElevatorMoves :: Floor -> [Floor]
validElevatorMoves One = [Two]
validElevatorMoves Two = [One, Three]
validElevatorMoves Three = [Two, Four]
validElevatorMoves Four = [Three]

friedChip :: S.Set Item -> Bool
friedChip s = any bad xs where
  xs = S.toList s

  bad (RTG _) = False
  bad (Chip lbl) = any isRTG xs && not (S.member (RTG lbl) s)

  isRTG (RTG _) = True
  isRTG _ = False

leaveOneOut :: [a] -> [(a, [a])]
leaveOneOut [] = []
leaveOneOut (x:xs) = (x, xs):(prepend x <$> leaveOneOut xs) where
  prepend y (a, as) = (a, y:as)

choose :: [a] -> Int -> [([a], [a])]
choose xs 0 = [([], xs)]
choose xs n = do
  (x, rest) <- leaveOneOut xs
  (group, rest') <- choose rest (n - 1)
  pure (x:group, rest')

-- moves of one or two items which Preserve Balance
-- this is where the Fun begins
validItemMoves :: S.Set Item -> S.Set Item -> [(S.Set Item, S.Set Item)]
validItemMoves from to = moveOne <> moveTwo where
  moveOne = do
    (chosen, rest) <- leaveOneOut $ S.toList from
    let from' = S.fromList rest
        to' = S.insert chosen to
    guard (not (friedChip from') && not (friedChip to'))
    pure (from', to')
  moveTwo = do
    (chosen, rest) <- choose (S.toList from) 2
    let from' = S.fromList rest
        to' = foldl' (flip S.insert) to chosen
    guard (not (friedChip from') && not (friedChip to'))
    pure (from', to')

validMoves :: State -> [State]
validMoves s = do
  let oldFloor = elevatorFloor s
  newFloor <- validElevatorMoves oldFloor
  (from', to') <- validItemMoves (floorItems oldFloor s) (floorItems newFloor s)
  let s' = s { elevatorFloor = newFloor }
      s'' = withFloor oldFloor from' s'
      s''' = withFloor newFloor to' s''
  pure s'''

-- all items on fourth floor
won :: State -> Bool
won s = elevatorFloor s == Four && S.null (firstFloor s)
  && S.null (secondFloor s) && S.null (thirdFloor s)

item :: Parser Item
item =  RTG <$> ("a " *> letters <* " generator")
    <|> Chip <$> ("a " *> letters <* "-compatible microchip")

items :: Parser (S.Set Item)
items =  S.fromList <$> sepBy sep item
     <|> S.empty <$ "nothing relevant"
  where
    sep = ", and " <|> " and " <|> ", "

world :: Parser State
world = do
  _ <- "The first floor contains "
  f1 <- items
  _ <- lexeme "."
  _ <- "The second floor contains "
  f2 <- items
  _ <- lexeme "."
  _ <- "The third floor contains "
  f3 <- items
  _ <- lexeme "."
  _ <- "The fourth floor contains "
  f4 <- items
  _ <- lexeme "."
  pure $ State One f1 f2 f3 f4

main :: IO ()
main = do
  Just state <- parseStdin world
  print $ validMoves state
