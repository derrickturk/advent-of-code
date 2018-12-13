import Text.Read (readMaybe)
import Data.List (foldl', findIndex)

data Layer = Layer Int Int

scannerPosition :: Int -> Int -> Int
scannerPosition range time =
  let cyclePos = time `mod` (range * 2 - 2)
   in if cyclePos < range
     then cyclePos
     else range - 2 - cyclePos `mod` range

severity :: Layer -> Int
severity (Layer range depth) =
  if scannerPosition range depth == 0
    then depth * range
    else 0

caughtWithDelay :: Int -> Layer -> Bool
caughtWithDelay delay (Layer range depth) =
  scannerPosition range (depth + delay) == 0

tripSeverity :: [Layer] -> Int
tripSeverity = sum . fmap severity

tripCaughtWithDelay :: [Layer] -> Int -> Bool
tripCaughtWithDelay layers delay = any id $ fmap (caughtWithDelay delay) layers

parseLayer :: String -> Maybe Layer
parseLayer s = case words s of
  [d, r] -> Layer <$> readMaybe r <*> readMaybe (init d)
  _ -> Nothing

main :: IO ()
main = do
  input <- traverse parseLayer . lines <$> getContents
  case input of
    Just layers -> do
      print $ tripSeverity layers
      print $ findIndex not $ tripCaughtWithDelay layers <$> [0..]
    Nothing -> putStrLn "invalid input"
