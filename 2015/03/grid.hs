import qualified Data.Set as S
import Data.List (foldl')

type Coord = (Integer, Integer)

newtype SantaState = SantaState { getSantaState :: (Coord, S.Set Coord) }
  deriving Show

move :: Char -> Coord -> Coord
move '^' (x, y) = (x, y + 1)
move 'v' (x, y) = (x, y - 1)
move '>' (x, y) = (x + 1, y)
move '<' (x, y) = (x - 1, y)
move _ c = c

initial :: SantaState
initial = SantaState ((0, 0), S.singleton (0, 0))

santaWalk :: SantaState -> String -> SantaState
santaWalk = foldl' step where
  step (SantaState (c, seen)) instr = let c' = move instr c
                                       in SantaState (c', (S.insert c' seen))

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x:odds xs

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs

main :: IO ()
main = do
  instrs <- getContents
  let (SantaState (_, seen)) = santaWalk initial instrs
  print $ S.size seen
  let (SantaState (_, santaSeen)) = santaWalk initial (evens instrs)
      (SantaState (_, roboSeen)) = santaWalk initial (odds instrs)
  print $ S.size $ S.union santaSeen roboSeen
