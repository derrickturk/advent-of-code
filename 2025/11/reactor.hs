import Control.Monad.State
import Data.List (uncons)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

type Reactor = M.Map String [String]

parse :: String -> Maybe Reactor
parse = go M.empty . lines where
  go m [] = pure m
  go m (line:rest) = do
    let toks = words line
    (from, to) <- uncons toks
    (from', ":") <- pure $ break (== ':') from
    let m' = M.insert from' to m
    go m' rest

paths :: Reactor -> String -> String -> [[String]]
paths r from to = do
  next <- fromMaybe [] $ r M.!? from
  if to == next
    then [[from, to]]
    else (from:) <$> paths r next to

numPathsMemo :: Reactor
             -> String
             -> String
             -> State (M.Map (String, String) Int) Int
numPathsMemo r from to = do
  known <- gets (M.!? (from, to))
  case known of
    Just n -> pure n
    Nothing -> do
      ans <- do
        if from == to
          then pure 1
          else do
            let next = fromMaybe [] $ r M.!? from
             in sum <$> traverse (\f -> numPathsMemo r f to) next
      modify (M.insert (from, to) ans)
      pure ans

part2Paths :: Reactor -> State (M.Map (String, String) Int) Int
part2Paths r = do
  svrDac <- numPathsMemo r "svr" "dac"
  svrFft <- numPathsMemo r "svr" "fft"
  fftDac <- numPathsMemo r "fft" "dac"
  dacFft <- numPathsMemo r "dac" "fft"
  dacOut <- numPathsMemo r "dac" "out"
  fftOut <- numPathsMemo r "fft" "out"
  pure $ svrDac * dacFft * fftOut + svrFft * fftDac * dacOut

main :: IO ()
main = do
  Just r <- parse <$> getContents
  print $ length $ paths r "you" "out"
  print $ evalState (part2Paths r) M.empty
