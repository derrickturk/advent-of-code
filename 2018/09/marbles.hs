import qualified Data.Sequence as S
import Data.Sequence (Seq(..))
import Control.Monad.State.Strict
import Data.Foldable (toList, foldMap)
import Text.Read (readMaybe)

newtype Marble = Marble { marbleValue :: Int }
  deriving (Eq, Show)

newtype Player = Player { playerNumber :: Int }
  deriving (Eq, Show)

data GameState = GameState { playerCount :: Int
                           , playerScores :: Seq Int
                           , marbles :: Seq Marble
                           , currentPlayer :: Player
                           } deriving Show

data RelRef = CW Int
            | CCW Int
            deriving (Show, Eq)

initialState :: Int -> GameState
initialState ps =
  GameState ps (S.replicate ps 0) (S.singleton (Marble 0)) (Player 0)

refocus :: RelRef -> Seq a -> Seq a
refocus (CW 0) s = s
refocus (CCW 0) s = s
refocus _ Empty = Empty
refocus (CCW n) (s :|> x) = refocus (CCW (n - 1)) (x :<| s)
refocus (CW n) (x :<| s) = refocus (CW (n - 1)) (s :|> x)

gameStep :: GameState -> Marble -> GameState
gameStep (GameState ps scores marbles (Player p)) (Marble v)
  | v `mod` 23 == 0 =
      let ((Marble m) :<| marbles') = refocus (CCW 7) marbles
          scores' = S.adjust (+ (v + m)) p scores
      in GameState ps scores' marbles' (Player $ (p + 1) `mod` ps)
  | otherwise =
      let marbles' = (Marble v) :<| (refocus (CW 2) marbles)
      in GameState ps scores marbles' (Player $ (p + 1) `mod` ps)

prettyGameState :: GameState -> String
prettyGameState (GameState _ scores marbles (Player p)) =
  "[" ++ show p ++ "]" ++ showMarbles marbles
  ++ " / scores = " ++ show (toList scores)
  where
    showMarbles Empty = ""
    showMarbles ((Marble m) :<| rest) = " (" ++ show m ++ ")"
      ++ foldMap showMarble' rest
    showMarble' (Marble m) = " " ++ show m

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
