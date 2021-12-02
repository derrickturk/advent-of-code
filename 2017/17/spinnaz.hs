import System.Environment (getArgs)
import Text.Read (readMaybe)

import Circ

spinStep :: Int -> Int -> Circ Int -> Circ Int
spinStep i n c = insert i (rotateNR n c)

spinGame :: Int -> [Circ Int]
spinGame n = snd <$> iterate f (1, singleton 0) where
  f (i, c) = (i + 1, spinStep i n c)

main :: IO ()
main = do
  [n] <- getArgs
  let Just n' = readMaybe n
  print $ focus $ rotateR $ spinGame 3 !! 2017
  print $ focus $ rotateR $ spinGame n' !! 2017
  {-
  case rotateTo 0 (spinGame n' !! 50000000) of
    Just c -> print $ focus $ rotateR c
    Nothing -> putStrLn "hmmm"
  -}
