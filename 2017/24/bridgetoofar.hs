import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)
import Control.Monad (guard)

import Combo (leaveOneOut)

type Bridge = (Int, Int)

validSteps :: Int -> [Bridge] -> [(Int, Bridge, [Bridge])]
validSteps exposed bs = do
  (b@(i, j), others) <- leaveOneOut bs
  guard $ i == exposed || j == exposed
  if i == exposed
    then pure (j, b, others)
    else pure (i, b, others)

strength :: Bridge -> Int
strength (x, y) = x + y

strongest :: Int -> [Bridge] -> Int
strongest = go 0 where
  go soFar port bs = maximum $ soFar:
    [ go (soFar + strength b) next others
    | (next, b, others) <- validSteps port bs
    ]

longest :: Int -> [Bridge] -> (Int, Int)
longest = go (0, 0) where
  go soFar@(l, s) port bs = maximum $ soFar:
    [ go (l + 1, s + strength b) next others
    | (next, b, others) <- validSteps port bs
    ]

parseBridge :: T.Text -> Maybe Bridge
parseBridge line = case T.split (== '/') line of
  [x, y] -> (,) <$> readMaybe (T.unpack x) <*> readMaybe (T.unpack y)
  _ -> Nothing

main :: IO ()
main = do
  Just bridges <- traverse parseBridge . T.lines <$> TIO.getContents
  print $ strongest 0 bridges
  print $ snd $ longest 0 bridges
