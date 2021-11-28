{-# LANGUAGE OverloadedStrings, TupleSections #-}

-- the notorious RTG

import qualified Data.Text as T
import qualified Data.Set as S
import Control.Monad (guard)
import Data.List (foldl', sort)

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

data State
  = State { elevatorFloor :: Floor
          , firstFloor :: S.Set Item
          , secondFloor :: S.Set Item
          , thirdFloor :: S.Set Item
          , fourthFloor :: S.Set Item
          , elements :: [T.Text]
          } deriving (Show, Eq, Ord)

type FastState = (Floor, [(Floor, Floor)])

data FastItem
   = FastChip Floor
   | FastRTG Floor
   | FastPair Floor
   deriving (Show, Eq, Ord)

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
validItemMoves from to = moveTwo <> moveOne where
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

fastValidItemMoves :: FastState -> Floor -> [FastState]
fastValidItemMoves (oldFloor, xs) newFloor = (newFloor,) . sort <$>
  movePair <>
  move2ChipToPair <>
  move1ChipToPair <>
  moveRTGs <>
  moveLonerChips
  where
    movePair = if ((oldFloor, oldFloor) `elem` xs)
      then [replace1 (oldFloor, oldFloor) (newFloor, newFloor) xs]
      else []

    pairableChips = length $ filter (== (oldFloor, newFloor)) xs

    move2ChipToPair = if pairableChips >= 2
      then replace1 (oldFloor, newFloor) (newFloor, newFloor) <$> move1ChipToPair
      else []

    move1ChipToPair = if pairableChips >= 1
      then [replace1 (oldFloor, newFloor) (newFloor, newFloor) xs]
      else []

    newFloorRTGs = length $ filter (\(_, rtgFloor) -> rtgFloor == newFloor) xs
    lonerableChips = filter (\(chipFloor, _) -> chipFloor == oldFloor) xs

    moveLonerChips = do
      guard $ newFloorRTGs == 0
      n <- [1..2]
      (toMove, _) <- choose lonerableChips n
      pure $ foldl'
        (\s old -> replace1 old (changeChipFloor newFloor old) s) xs toMove

    movableRTGs = filter rtgMovable xs

    rtgMovable (chipFloor, rtgFloor) = rtgFloor == oldFloor
      && chipFloor /= oldFloor && all (wontFry oldFloor newFloor) xs

    wontFry oldRTGFloor newRTGFloor (chipFloor, rtgFloor)
      | chipFloor /= newRTGFloor = True
      | oldRTGFloor == rtgFloor = True -- moving into a pair
      | otherwise = False

    moveRTGs = do
      n <- [1..2]
      (toMove, _) <- choose movableRTGs n
      pure $ foldl'
        (\s old -> replace1 old (changeRTGFloor newFloor old) s) xs toMove

    changeChipFloor chipFloor (_, rtgFloor) = (chipFloor, rtgFloor)
    changeRTGFloor rtgFloor (chipFloor, _) = (chipFloor, rtgFloor)

    replace1 _ _ [] = []
    replace1 x y (z:zs)
      | x == z = y:zs
      | otherwise = z:replace1 x y zs

validMoves :: State -> [State]
validMoves s = do
  let oldFloor = elevatorFloor s
  newFloor <- validElevatorMoves oldFloor
  (from', to') <- validItemMoves (floorItems oldFloor s) (floorItems newFloor s)
  let s' = s { elevatorFloor = newFloor }
      s'' = withFloor oldFloor from' s'
      s''' = withFloor newFloor to' s''
  pure s'''

fastValidMoves :: FastState -> [FastState]
fastValidMoves s@(oldFloor, _) = do
  newFloor <- validElevatorMoves oldFloor
  fastValidItemMoves s newFloor

-- all items on fourth floor
won :: State -> Bool
won s = elevatorFloor s == Four && S.null (firstFloor s)
  && S.null (secondFloor s) && S.null (thirdFloor s)

fastWon :: FastState -> Bool
fastWon (Four, xs) = all (== (Four, Four)) xs
fastWon _ = False

toFast :: State -> FastState
toFast s = (elevatorFloor s, sort $ fastItems <$> elements s) where
  fastItems e = (chipFloor e, rtgFloor e)
  chipFloor e = if S.member (Chip e) (firstFloor s)
    then One
    else if S.member (Chip e) (secondFloor s)
      then Two
      else if S.member (Chip e) (thirdFloor s)
        then Three
        else if S.member (Chip e) (fourthFloor s)
          then Four
          else error "well, this problem is hosed"
  rtgFloor e = if S.member (RTG e) (firstFloor s)
    then One
    else if S.member (RTG e) (secondFloor s)
      then Two
      else if S.member (RTG e) (thirdFloor s)
        then Three
        else if S.member (RTG e) (fourthFloor s)
          then Four
          else error "well, this problem is hosed"

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
  let es = S.toList $ S.map getTag $ f1 <> f2 <> f3 <> f4
  pure $ State One f1 f2 f3 f4 es
  where
    getTag (RTG t) = t
    getTag (Chip t) = t

main :: IO ()
main = do
  Just state <- parseStdin world
  print state
  let fastState = toFast state
  -- print $ costToWin state (\s -> zip (repeat (1 :: Int)) (validMoves s)) won
  print $ statesToWin
    fastState
    (\s -> zip (repeat (1 :: Int)) (fastValidMoves s))
    fastWon
