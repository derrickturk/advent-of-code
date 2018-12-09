import qualified Data.Sequence as S
import Control.Monad.State.Strict
import Data.Foldable (toList)
import Text.Read (readMaybe)

newtype Marble = Marble { marbleValue :: Int }
  deriving (Eq, Show)

newtype Player = Player { playerNumber :: Int }
  deriving (Eq, Show)

data GameState = GameState { playerCount :: Int
                           , playerScores :: S.Seq Int
                           , marbles :: S.Seq Marble
                           , currentIndex :: Int
                           , currentPlayer :: Player
                           } deriving Show

data RelRef = CW Int
            | CCW Int
            deriving (Show, Eq)

initialState :: Int -> GameState
initialState ps =
  GameState ps (S.replicate ps 0) (S.singleton (Marble 0)) 0 (Player 0)

relIndex :: S.Seq a -> Int -> RelRef -> Int
relIndex s i (CW n) = (i + n) `mod` S.length s
relIndex s i (CCW n) = (i - n) `mod` S.length s

relTake :: S.Seq a -> Int -> RelRef -> (a, S.Seq a)
relTake s i rel =
  let i' = relIndex s i rel
  in (s `S.index` i', S.deleteAt i' s)

relInsert :: S.Seq a -> Int -> RelRef -> a -> (Int, S.Seq a)
relInsert s i rel x =
  let i' = relIndex s i rel
  in (i', S.insertAt (relIndex s i rel) x s)

gameStep :: GameState -> Marble -> GameState
gameStep (GameState ps scores marbles i (Player p)) (Marble v)
  | v `mod` 23 == 0 =
      let (Marble m, marbles') = relTake marbles i (CCW 7)
          i' = relIndex marbles' i (CCW 7)
          scores' = S.adjust (+ (v + m)) p scores
      in GameState ps scores' marbles' i' (Player $ (p + 1) `mod` ps)
  | otherwise =
      let (i', marbles') = relInsert marbles i (CW 2) (Marble v)
      in GameState ps scores marbles' i' (Player $ (p + 1) `mod` ps)

prettyGameState :: GameState -> String
prettyGameState (GameState _ scores marbles i (Player p)) =
  "[" ++ show p ++ "]" ++ S.foldMapWithIndex (showMarble i) marbles
  ++ " / scores = " ++ show (toList scores)
  where
    showMarble cur i (Marble v)
      | i == cur = " (" ++ show v ++ ")"
      | otherwise = " " ++ show v

runGame :: Int -> [GameState]
runGame ps = runGame' (initialState ps) (Marble 1) where
  runGame' s m = let next = gameStep s m
                 in s : runGame' next (Marble $ marbleValue m + 1)

gameAfterMarble :: Int -> Marble -> GameState
gameAfterMarble ps (Marble v) = runGame ps !! v

parseDesc :: String -> Maybe (Int, Marble)
parseDesc s = case words s of
  [p, _, _, _, _, _, m, _] -> (,) <$> readMaybe p <*> (Marble <$> readMaybe m)
  _ -> Nothing

main :: IO ()
main = do
  input <- parseDesc <$> getContents
  case input of
    Just (p, m) -> print $ maximum $ playerScores $ gameAfterMarble p m
    Nothing -> putStrLn "invalid input"
