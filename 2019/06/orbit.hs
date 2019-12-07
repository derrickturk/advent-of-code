{-# LANGUAGE OverloadedStrings #-}

import Text.Read (readMaybe)
import System.IO (stderr)
import System.Environment (getArgs)

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- (child, parent)
type Orbit = (T.Text, T.Text)
type OrbitMap = M.Map T.Text T.Text

parseOrbit :: T.Text -> Maybe Orbit
parseOrbit line = case T.splitOn ")" line of
  [parent, child] -> Just (child, parent)
  _ -> Nothing

orbitMap :: [Orbit] -> OrbitMap
orbitMap = M.fromList
{-# INLINE orbitMap #-}

totalOrbits :: OrbitMap -> T.Text -> Int
totalOrbits map body = go 0 (M.lookup body map) where
  go total Nothing = total
  go total (Just other) = go (total + 1) (M.lookup other map)
{-# INLINE totalOrbits #-}

problem1 :: OrbitMap -> IO ()
problem1 map = print $ sum $ totalOrbits map <$> M.keys map

main :: IO ()
main = do
  args <- getArgs
  let action = case args of
        ["1"] -> Just problem1
        _ -> Nothing
  case action of
    Just m -> do
      contents <- TIO.getContents
      let orbits = orbitMap <$> (traverse parseOrbit $ T.lines contents)
      case orbits of
        Just map -> m map
        _ -> TIO.hPutStrLn stderr "Invalid input."
    Nothing -> TIO.hPutStrLn stderr "Usage: orbit 1|2 <input.txt"
