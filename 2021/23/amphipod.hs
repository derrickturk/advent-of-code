{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (guard)
import Data.List (find, transpose)
import Data.Maybe (isJust, maybeToList)
import Data.Array

import AStar

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
      rm = rms ! i
      (_, btm) = bounds rm
      wrongBug k = case rm ! k of
        Just b' -> b /= b'
        Nothing -> False
  j <- maybeToList $ bottomEmpty rm
  guard $ not $ any wrongBug [j + 1..btm]
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
      let steps = 2 + j + j' + abs (roomToHall i' - roomToHall i)
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

parseWorld :: [String] -> Maybe World
parseWorld (_:(_:hall):rooms) =
  (,) <$> parseRooms rooms <*> pure (listArray (0, 10) $ parseBug <$> init hall)
  where
    parseRooms :: [String] -> Maybe Rooms
    parseRooms = fmap (toArrays . transpose) . traverse parseRoom . init

    parseRoom :: [Char] -> Maybe [Maybe Bug]
    parseRoom (_:_:_:a:_:b:_:c:_:d:_) =
      Just [parseBug a, parseBug b, parseBug c, parseBug d]
    parseRoom _ = Nothing

    parseBug :: Char -> Maybe Bug
    parseBug 'A' = Just A
    parseBug 'B' = Just B
    parseBug 'C' = Just C
    parseBug 'D' = Just D
    parseBug _ = Nothing

    toArrays :: [[Maybe Bug]] -> Rooms
    toArrays cols = listArray (0, 3) $ toRoom <$> cols

    toRoom :: [Maybe Bug] -> Room
    toRoom col = listArray (0, length col - 1) col
parseWorld _ = Nothing

main :: IO ()
main = do
  Just world <- parseWorld . lines <$> getContents
  print $ costToWin world validMoves heuristic won

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
