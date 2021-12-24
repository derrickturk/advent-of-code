{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (guard)
import Data.List (find)
import Data.Maybe (isJust, maybeToList)
import Data.Array

import AStar
-- import Dijkstra
import FemtoParsec

data Bug
  = A | B | C | D
  deriving (Eq, Show, Ord)

type Room = Array Int (Maybe Bug)
type Rooms = Array Int Room
type Hallway = Array Int (Maybe Bug)
type World = (Rooms, Hallway)

energy :: Bug -> Int
energy A = 1
energy B = 10
energy C = 100
energy D = 1000

bugRoom :: Bug -> Int
bugRoom A = 0
bugRoom B = 1
bugRoom C = 2
bugRoom D = 3

hallway :: Hallway
hallway = listArray (0, 10) $ repeat Nothing

roomToHall :: Int -> Int
roomToHall = (* 2) . (+ 1)

hallToRoom :: Int -> Maybe Int
hallToRoom 2 = Just 0
hallToRoom 4 = Just 1
hallToRoom 6 = Just 2
hallToRoom 8 = Just 3
hallToRoom _ = Nothing

isFoyer :: Int -> Bool
isFoyer = isJust . hallToRoom

hallTargets :: Hallway -> Int -> [Int]
hallTargets hall i =
  let i' = roomToHall i
      goDown j
        | j < 0 || j > 10 || isJust (hall ! j) = []
        | isFoyer j = goDown (j - 1)
        | otherwise = j:goDown (j - 1)
      goUp j
        | j < 0 || j > 10 || isJust (hall ! j) = []
        | isFoyer j = goUp (j + 1)
        | otherwise = j:goUp (j + 1)
   in if isJust (hall ! i')
     then []
     else goDown (i' - 1) <> goUp (i' + 1)

clearBetween :: Hallway -> Int -> Int -> Bool
clearBetween hall i j = all ((== Nothing) . (hall !)) $
  if i < j then [i..j] else [j..i]

-- don't check i, that's where we are!
clearBetween1 :: Hallway -> Int -> Int -> Bool
clearBetween1 hall i j = all ((== Nothing) . (hall !)) $
  if i < j then [i+1..j] else [j..i-1]

topOccupied :: Room -> Maybe (Int, Bug)
topOccupied rm = case find (isJust . snd) $ assocs rm of
  Just (i, Just bug) -> Just (i, bug)
  Nothing -> Nothing
  _ -> error "impossible"

bottomEmpty :: Room -> Maybe Int
bottomEmpty rm = go Nothing $ assocs rm where
  go _ ((i, Nothing):rest) = go (Just i) rest
  go dst _ = dst

-- TODO: split into roomToRoomSources, roomToHallSources?
roomSources :: Rooms -> [(Int, Int, Bug)]
roomSources rms = do
  (i, rm) <- assocs rms
  (j, bug) <- maybeToList $ topOccupied rm
  let (_, btm) = bounds rm
      allowsLeaving k = case rm ! k of
        Nothing -> True
        Just bug' -> bugRoom bug' /= i
  guard $ bugRoom bug /= i || any allowsLeaving [j + 1..btm]
  pure (i, j, bug)

roomTargets :: Bug -> Rooms -> [(Int, Int)]
roomTargets b rms = do
  let i = bugRoom b
  j <- maybeToList $ bottomEmpty $ rms ! i
  pure (i, j)

validMoves :: World -> [(Int, World)]
validMoves (rms, hall) =
  roomToRoomMoves <>
  roomToHallMoves <>
  hallToRoomMoves
  where
    roomToRoomMoves = do
      (i, j, bug) <- roomSources rms
      (i', j') <- roomTargets bug rms
      guard $ i /= i'
      guard $ clearBetween hall (roomToHall i) (roomToHall i')
      let steps = 2 + j + j' + abs (roomToHall i' - roomToHall j')
          src' = rms ! i // [(j, Nothing)]
          dst' = rms ! i' // [(j', Just bug)]
          rms' = rms // [(i, src'), (i', dst')]
      pure (energy bug * steps, (rms', hall))

    roomToHallMoves = do
      (i, j, bug) <- roomSources rms
      k <- hallTargets hall i
      let steps = 1 + j + abs (roomToHall i - k)
          src' = rms ! i // [(j, Nothing)]
          rms' = rms // [(i, src')]
          hall' = hall // [(k, Just bug)]
      pure (energy bug * steps, (rms', hall'))

    hallToRoomMoves = do
      (k, Just bug) <- assocs hall
      (i, j) <- roomTargets bug rms
      guard $ clearBetween1 hall k (roomToHall i)
      let steps = 1 + j + abs (k - roomToHall i)
          dst' = rms ! i // [(j, Just bug)]
          rms' = rms // [(i, dst')]
          hall' = hall // [(k, Nothing)]
      pure (energy bug * steps, (rms', hall'))

heuristic :: World -> Int
heuristic (rms, hall) = sum roomBugs + sum hallBugs where
  roomBugs = do
    (i, rm) <- assocs rms
    (j, Just bug) <- assocs rm
    let i' = bugRoom bug
    pure $ if i /= i'
      then energy bug * (2 + j + abs (roomToHall i - roomToHall i'))
      else 0

  hallBugs = do
    (i, Just bug) <- assocs hall
    let j = bugRoom bug
    pure $ energy bug * (1 + abs (i - roomToHall j))

won :: World -> Bool
won (rms, _) =
  all (== Just A) (rms ! 0) &&
  all (== Just B) (rms ! 1) &&
  all (== Just C) (rms ! 2) &&
  all (== Just D) (rms ! 3)

bugKind :: Parser Bug
bugKind = A <$ "A" <|> B <$ "B" <|> C <$ "C" <|> D <$ "D"

rooms :: Parser Rooms
rooms = do
  _ <- lexeme "#############"
  _ <- lexeme "#...........#"
  _ <- "###"
  pod0 <- bugKind
  _ <- "#"
  pod1 <- bugKind
  _ <- "#"
  pod2 <- bugKind
  _ <- "#"
  pod3 <- bugKind
  _ <- lexeme "###"
  _ <- "#"
  pod4 <- bugKind
  _ <- "#"
  pod5 <- bugKind
  _ <- "#"
  pod6 <- bugKind
  _ <- "#"
  pod7 <- bugKind
  _ <- lexeme "#"
  _ <- lexeme "#########"
  pure $ listArray (0, 3) [ listArray (0, 1) [Just pod0, Just pod4]
                          , listArray (0, 1) [Just pod1, Just pod5]
                          , listArray (0, 1) [Just pod2, Just pod6]
                          , listArray (0, 1) [Just pod3, Just pod7]
                          ]

expandWorld :: World -> World
expandWorld (rms, hall) =
  let rms' = listArray (0, 3)
        [ listArray (0, 3) [(rms ! 0) ! 0, Just D, Just D, (rms ! 0) ! 1]
        , listArray (0, 3) [(rms ! 1) ! 0, Just C, Just B, (rms ! 1) ! 1]
        , listArray (0, 3) [(rms ! 2) ! 0, Just B, Just A, (rms ! 2) ! 1]
        , listArray (0, 3) [(rms ! 3) ! 0, Just A, Just C, (rms ! 3) ! 1]
        ]
   in (rms', hall)

main :: IO ()
main = do
  Just initRooms <- parseStdin rooms
  let world = (initRooms, hallway)
      world' = expandWorld world
  -- print $ costToWin world validMoves heuristic won
  print $ costToWin world' validMoves heuristic won

  {-
  -- 12481 = 12081 + 400
  -- let world = (array (0,3) [(0,(Just B,Just A)),(1,(Just C,Just D)),(2,(Nothing,Just C)),(3,(Just D,Just A))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Just B),(4,Nothing),(5,Nothing),(6,Nothing),(7,Nothing),(8,Nothing),(9,Nothing),(10,Nothing)])
  -- 12081 = 9081 + 3000
  -- let world = (array (0,3) [(0,(Just B,Just A)),(1,(Nothing,Just D)),(2,(Just C,Just C)),(3,(Just D,Just A))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Just B),(4,Nothing),(5,Nothing),(6,Nothing),(7,Nothing),(8,Nothing),(9,Nothing),(10,Nothing)])
  -- 9081 = 9051 + 30
  --let world = (array (0,3) [(0,(Just B,Just A)),(1,(Nothing,Nothing)),(2,(Just C,Just C)),(3,(Just D,Just A))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Just B),(4,Nothing),(5,Just D),(6,Nothing),(7,Nothing),(8,Nothing),(9,Nothing),(10,Nothing)])
  -- 9051 = 9011 + 40
  -- let world = (array (0,3) [(0,(Just B,Just A)),(1,(Nothing,Just B)),(2,(Just C,Just C)),(3,(Just D,Just A))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Just D),(6,Nothing),(7,Nothing),(8,Nothing),(9,Nothing),(10,Nothing)])
  -- 9011 = 2003 + 7008
  -- let world = (array (0,3) [(0,(Nothing,Just A)),(1,(Just B,Just B)),(2,(Just C,Just C)),(3,(Just D,Just A))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Just D),(6,Nothing),(7,Nothing),(8,Nothing),(9,Nothing),(10,Nothing)])
  -- let world = (array (0,3) [(0,(Nothing,Just A)),(1,(Just B,Just B)),(2,(Just C,Just C)),(3,(Nothing,Just A))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Just D),(6,Nothing),(7,Just D),(8,Nothing),(9,Nothing),(10,Nothing)])
  -- 7008 = 7000 + 8
  -- let world = (array (0,3) [(0,(Nothing,Just A)),(1,(Just B,Just B)),(2,(Just C,Just C)),(3,(Nothing,Nothing))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Just D),(6,Nothing),(7,Just D),(8,Nothing),(9,Just A),(10,Nothing)])
  -- let world = (array (0,3) [(0,(Nothing,Just A)),(1,(Just B,Just B)),(2,(Just C,Just C)),(3,(Nothing,Just D))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Just D),(6,Nothing),(7,Nothing),(8,Nothing),(9,Just A),(10,Nothing)])
  -- 8
  -- let world = (array (0,3) [(0,(Nothing,Just A)),(1,(Just B,Just B)),(2,(Just C,Just C)),(3,(Just D,Just D))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Nothing),(6,Nothing),(7,Nothing),(8,Nothing),(9,Just A),(10,Nothing)])
  -- let world = (array (0,3) [(0,(Just A,Just A)),(1,(Just B,Just B)),(2,(Just C,Just C)),(3,(Just D,Just D))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Nothing),(6,Nothing),(7,Nothing),(8,Nothing),(9,Nothing),(10,Nothing)])
  print $ validMoves world
  print $ won world
  -}

  {-
  let unfuck (rms, hall) = (fmap unfuck' rms, hall)
      unfuck' :: (Maybe Bug, Maybe Bug) -> Array Int (Maybe Bug)
      unfuck' (shit, fuck) = listArray (0,1) [shit, fuck]
      world :: World
      world = unfuck $ (array (0,3) [(0,(Just B,Just A)),(1,(Just C,Just D)),(2,(Nothing,Just C)),(3,(Just D,Just A))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Just B),(4,Nothing),(5,Nothing),(6,Nothing),(7,Nothing),(8,Nothing),(9,Nothing),(10,Nothing)])
  print $ validMoves world
  print $ heuristic world
  print $ costToWin world validMoves heuristic won
  -}
