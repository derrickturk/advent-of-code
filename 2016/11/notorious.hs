{-# LANGUAGE DataKinds, OverloadedStrings, StandaloneDeriving, TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies, FlexibleContexts, UndecidableInstances #-}

-- the notorious RTG

import qualified Data.Text as T
import qualified Data.Set as S
import Control.Monad (guard)
import Data.List (foldl')
import Data.Kind (Type)
import Prelude hiding (null)

import FemtoParsec
import Dijkstra

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

-- Make Illegal States Unrepresentable Again, Or Don't, Jesus
data FastFloor
  = LoneChips [Floor] -- original floor of corresponding RTG
  | PairsAndRTGs [Floor] [Floor] -- original floor (sigh) of RTG
  deriving (Show, Eq, Ord)

data ReprKind
  = Normie
  | Fast

class FloorRepr (a :: ReprKind) where
  type Repr a = (r :: Type) | r -> a
  validItemMoves :: Repr a -> Repr a -> [(Repr a, Repr a)]
  null :: Repr a -> Bool

data State (a :: ReprKind)
  = State { elevatorFloor :: Floor
          , firstFloor :: Repr a
          , secondFloor :: Repr a
          , thirdFloor :: Repr a
          , fourthFloor :: Repr a
          }

deriving instance Show (Repr a) => Show (State a)
deriving instance Eq (Repr a) => Eq (State a)
deriving instance Ord (Repr a) => Ord (State a)

floorItems :: Floor -> State a -> Repr a
floorItems One = firstFloor
floorItems Two = secondFloor
floorItems Three = thirdFloor
floorItems Four = fourthFloor

withFloor :: Floor -> Repr a -> State a -> State a
withFloor One contents s = s { firstFloor = contents }
withFloor Two contents s = s { secondFloor = contents }
withFloor Three contents s = s { thirdFloor = contents }
withFloor Four contents s = s { fourthFloor = contents }

validElevatorMoves :: Floor -> [Floor]
validElevatorMoves One = [Two]
validElevatorMoves Two = [One, Three]
validElevatorMoves Three = [Two, Four]
validElevatorMoves Four = [Three]

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

fried :: Repr 'Normie -> Bool
fried s = any bad xs where
  xs = S.toList s
  bad (RTG _) = False
  bad (Chip lbl) = any isRTG xs && not (S.member (RTG lbl) s)
  isRTG (RTG _) = True
  isRTG _ = False

instance FloorRepr 'Normie where
  type Repr 'Normie = S.Set Item

  -- moves of one or two items which Preserve Balance
  -- this is where the Fun begins
  validItemMoves from to = moveTwo <> moveOne where
    moveOne = do
      (chosen, rest) <- leaveOneOut $ S.toList from
      let from' = S.fromList rest
          to' = S.insert chosen to
      guard (not (fried from') && not (fried to'))
      pure (from', to')
    moveTwo = do
      (chosen, rest) <- choose (S.toList from) 2
      let from' = S.fromList rest
          to' = foldl' (flip S.insert) to chosen
      guard (not (fried from') && not (fried to'))
      pure (from', to')

  null = S.null

instance FloorRepr 'Fast where
  type Repr 'Fast = FastFloor

  validItemMoves (LoneChips xs) (LoneChips ys) = do
    n <- [2,1]
    (toMove, xs') <- choose xs n
    pure (LoneChips xs', LoneChips $ ys <> toMove)

  validItemMoves (PairsAndRTGs xp xr) (PairsAndRTGs yp yr) = do
    nr <- [2, 1, 0]

    = movePair <> moveRTGs
    where
      movePair = guard (xp > 0) [(PairsAndRTGs (xp - 1) xr, PairsAndRTGs (yp + 1) yr]
    np <- [2, 1, 0]
    let nr = 2 - np
    guard $ np <= xp && nr <= xr
    pure (PairsAndRTGs (xp - np) (xr - nr), PairsAndRTGs (yp + np) (yr + nr))

  validItemMoves (LoneChips xs) (PairsAndRTGs 0 []) = do
    n <- [2, 1]
    (toMove, xs') <- choose xs n
    pure (LoneChips xs', LoneChips toMove)

  {-
  validItemMoves (_, LoneChips xs) (f2, PairsAndRTGs np nr) = do
    let moveable = filter (== f2) xs
    n <- [2, 1]
    (toMove, xs') <- choose moveable n
    let moved = length toMove
    pure (LoneChips xs', PairsAndRTGs (np + moved) (nr - moved))
  -}

  null (LoneChips []) = True
  null (PairsAndRTGs 0 0) = True
  null _ = False

validMoves :: FloorRepr a => State a -> [State a]
validMoves s = do
  let oldFloor = elevatorFloor s
  newFloor <- validElevatorMoves oldFloor
  (from', to') <- validItemMoves
    (oldFloor, (floorItems oldFloor s)) (newFloor, (floorItems newFloor s))
  let s' = s { elevatorFloor = newFloor }
      s'' = withFloor oldFloor from' s'
      s''' = withFloor newFloor to' s''
  pure s'''

-- all items on fourth floor
won :: FloorRepr a => State a -> Bool
won s = elevatorFloor s == Four && null (firstFloor s)
  && null (secondFloor s) && null (thirdFloor s)

item :: Parser Item
item =  RTG <$> ("a " *> letters <* " generator")
    <|> Chip <$> ("a " *> letters <* "-compatible microchip")

items :: Parser (S.Set Item)
items =  S.fromList <$> sepBy sep item
     <|> S.empty <$ "nothing relevant"
  where
    sep = ", and " <|> " and " <|> ", "

world :: Parser (State 'Normie)
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
  print $ costToWin state (\s -> zip (repeat (1 :: Int)) (validMoves s)) won
