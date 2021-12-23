{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (guard)
import Data.Array

-- import AStar
import Dijkstra
import FemtoParsec

data Bug
  = A | B | C | D
  deriving (Eq, Show, Ord)

type Room = (Maybe Bug, Maybe Bug)
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

isFoyer :: Int -> Bool
isFoyer 2 = True
isFoyer 4 = True
isFoyer 6 = True
isFoyer 8 = True
isFoyer _ = False

roomToHall :: Int -> Int
roomToHall = (* 2) . (+ 1)

clearBetween :: Hallway -> Int -> Int -> Bool
clearBetween hall i j = all ((== Nothing) . (hall !)) $
  if i < j then [i..j] else [j..i]

-- don't check i, that's where we are!
clearBetween1 :: Hallway -> Int -> Int -> Bool
clearBetween1 hall i j = all ((== Nothing) . (hall !)) $
  if i < j then [i+1..j] else [j..i-1]

validMoves :: World -> [(Int, World)]
validMoves (rms, hall) =
  roomTopToBottom <>
  roomTopToTop <>
  roomBottomToBottom <>
  roomBottomToTop <>
  hallwayBugToRoomTop <>
  hallwayBugToRoomBottom <>
  roomTopBugToHall <>
  roomBottomBugToHall <>
  roomTopBugDance <>
  roomBottomBugDance
  where
    roomTopToTop = do
      (i, (Just bug, other1)) <- assocs rms
      let j = bugRoom bug
      case rms ! j of
        (Nothing, other2) -> do
          guard $ clearBetween hall (roomToHall i) (roomToHall j)
          guard $ other2 == Nothing || other2 == Just bug
          let steps = 2 + abs (roomToHall i - roomToHall j)
          pure ( energy bug * steps
               , (rms // [(i, (Nothing, other1)), (j, (Just bug, other2))], hall)
               )
        _ -> empty

    roomTopToBottom = do
      (i, (Just bug, other)) <- assocs rms
      let j = bugRoom bug
      case rms ! j of
        (Nothing, Nothing) -> do
          guard $ clearBetween hall (roomToHall i) (roomToHall j)
          let steps = 3 + abs (roomToHall i - roomToHall j)
          pure ( energy bug * steps
               , (rms // [(i, (Nothing, other)), (j, (Nothing, Just bug))], hall)
               )
        _ -> empty

    roomBottomToTop = do
      (i, (Nothing, Just bug)) <- assocs rms
      let j = bugRoom bug
      case rms ! j of
        (Nothing, other) -> do
          guard $ clearBetween hall (roomToHall i) (roomToHall j)
          guard $ other == Nothing || other == Just bug
          let steps = 3 + abs (roomToHall i - roomToHall j)
          pure ( energy bug * steps
               , (rms // [(i, (Nothing, Nothing)), (j, (Just bug, other))], hall)
               )
        _ -> empty

    roomBottomToBottom = do
      (i, (Nothing, Just bug)) <- assocs rms
      let j = bugRoom bug
      case rms ! j of
        (Nothing, Nothing) -> do
          guard $ clearBetween hall (roomToHall i) (roomToHall j)
          let steps = 4 + abs (roomToHall i - roomToHall j)
          pure ( energy bug * steps
               , (rms // [(i, (Nothing, Nothing)), (j, (Nothing, Just bug))], hall)
               )
        _ -> empty

    roomTopBugDance = do
      (i, (Just bug, Nothing)) <- assocs rms
      pure (energy bug, (rms // [(i, (Nothing, Just bug))], hall))

    roomBottomBugDance = do
      (i, (Nothing, Just bug)) <- assocs rms
      pure (energy bug, (rms // [(i, (Just bug, Nothing))], hall))

    roomTopBugToHall = do
      (i, (Just bug, other)) <- assocs rms
      (j, Nothing) <- assocs hall
      guard $ not $ isFoyer j
      guard $ clearBetween hall (roomToHall i) j
      let steps = 1 + abs (roomToHall i - j)
      pure ( energy bug * steps
           , (rms // [(i, (Nothing, other))], hall // [(j, Just bug)])
           )

    roomBottomBugToHall = do
      (i, (Nothing, Just bug)) <- assocs rms
      (j, Nothing) <- assocs hall
      guard $ not $ isFoyer j
      guard $ clearBetween hall (roomToHall i) j
      let steps = 2 + abs (roomToHall i - j)
      pure ( energy bug * steps
           , (rms // [(i, (Nothing, Nothing))], hall // [(j, Just bug)])
           )

    hallwayBugToRoomTop = do
      (i, (Nothing, other)) <- assocs rms
      (j, Just bug) <- assocs hall
      guard $ i == bugRoom bug
      guard $ clearBetween1 hall j (roomToHall i)
      guard $ other == Just bug || other == Nothing -- must not be different bug
      let steps = 1 + abs (roomToHall i - j)
      pure ( energy bug * steps
           , (rms // [(i, (Just bug, other))], hall // [(j, Nothing)])
           )

    hallwayBugToRoomBottom = do
      (i, (Nothing, Nothing)) <- assocs rms
      (j, Just bug) <- assocs hall
      guard $ i == bugRoom bug
      guard $ clearBetween1 hall j (roomToHall i)
      let steps = 2 + abs (roomToHall i - j)
      pure ( energy bug * steps
           , (rms // [(i, (Nothing, Just bug))], hall // [(j, Nothing)])
           )

heuristic :: World -> Int
heuristic (rms, hall) = sum topBugs + sum bottomBugs + sum hallBugs where
  topBugs = do
    (i, (Just bug, _)) <- assocs rms
    let j = bugRoom bug
    pure $ energy bug * (2 + abs (roomToHall i - roomToHall j))

  bottomBugs = do
    (i, (_, Just bug)) <- assocs rms
    let j = bugRoom bug
    pure $ energy bug * (3 + abs (roomToHall i - roomToHall j))

  hallBugs = do
    (i, Just bug) <- assocs hall
    let j = bugRoom bug
    pure $ energy bug * (1 + abs (i - roomToHall j))

won :: World -> Bool
won (rms, _) =
  (all (== (Just A)) $ rms ! 0) &&
  (all (== (Just B)) $ rms ! 1) &&
  (all (== (Just C)) $ rms ! 2) &&
  (all (== (Just D)) $ rms ! 3)

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
  pure $ listArray (0, 3) [ (Just pod0, Just pod4)
                          , (Just pod1, Just pod5)
                          , (Just pod2, Just pod6)
                          , (Just pod3, Just pod7)
                          ]

main :: IO ()
main = do
  Just initRooms <- parseStdin rooms
  let world = (initRooms, hallway)
  print $ costToWin world validMoves won

  {-
  -- let world = (array (0,3) [(0,(Just B,Just A)),(1,(Just C,Just D)),(2,(Nothing,Just C)),(3,(Just D,Just A))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Just B),(4,Nothing),(5,Nothing),(6,Nothing),(7,Nothing),(8,Nothing),(9,Nothing),(10,Nothing)])
  -- let world = (array (0,3) [(0,(Just B,Just A)),(1,(Nothing,Just D)),(2,(Just C,Just C)),(3,(Just D,Just A))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Just B),(4,Nothing),(5,Nothing),(6,Nothing),(7,Nothing),(8,Nothing),(9,Nothing),(10,Nothing)])
  --let world = (array (0,3) [(0,(Just B,Just A)),(1,(Nothing,Nothing)),(2,(Just C,Just C)),(3,(Just D,Just A))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Just B),(4,Nothing),(5,Just D),(6,Nothing),(7,Nothing),(8,Nothing),(9,Nothing),(10,Nothing)])
  -- let world = (array (0,3) [(0,(Just B,Just A)),(1,(Nothing,Just B)),(2,(Just C,Just C)),(3,(Just D,Just A))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Just D),(6,Nothing),(7,Nothing),(8,Nothing),(9,Nothing),(10,Nothing)])
  -- let world = (array (0,3) [(0,(Nothing,Just A)),(1,(Just B,Just B)),(2,(Just C,Just C)),(3,(Just D,Just A))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Just D),(6,Nothing),(7,Nothing),(8,Nothing),(9,Nothing),(10,Nothing)])
  -- let world = (array (0,3) [(0,(Nothing,Just A)),(1,(Just B,Just B)),(2,(Just C,Just C)),(3,(Nothing,Just A))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Just D),(6,Nothing),(7,Just D),(8,Nothing),(9,Nothing),(10,Nothing)])
  -- let world = (array (0,3) [(0,(Nothing,Just A)),(1,(Just B,Just B)),(2,(Just C,Just C)),(3,(Nothing,Nothing))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Just D),(6,Nothing),(7,Just D),(8,Nothing),(9,Just A),(10,Nothing)])
  -- let world = (array (0,3) [(0,(Nothing,Just A)),(1,(Just B,Just B)),(2,(Just C,Just C)),(3,(Nothing,Just D))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Just D),(6,Nothing),(7,Nothing),(8,Nothing),(9,Just A),(10,Nothing)])
  -- let world = (array (0,3) [(0,(Nothing,Just A)),(1,(Just B,Just B)),(2,(Just C,Just C)),(3,(Just D,Just D))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Nothing),(6,Nothing),(7,Nothing),(8,Nothing),(9,Just A),(10,Nothing)])
  let world = (array (0,3) [(0,(Just A,Just A)),(1,(Just B,Just B)),(2,(Just C,Just C)),(3,(Just D,Just D))],array (0,10) [(0,Nothing),(1,Nothing),(2,Nothing),(3,Nothing),(4,Nothing),(5,Nothing),(6,Nothing),(7,Nothing),(8,Nothing),(9,Nothing),(10,Nothing)])
  print $ validMoves world
  print $ won world
  -}
