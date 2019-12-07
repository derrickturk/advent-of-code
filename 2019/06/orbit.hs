{-# LANGUAGE OverloadedStrings #-}

import Data.List (elemIndex)
import System.IO (stderr)
import System.Environment (getArgs)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
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

allAncestors :: OrbitMap -> T.Text -> [T.Text]
allAncestors map body = body:rest where
  rest = case M.lookup body map of
    Nothing -> []
    Just other -> allAncestors map other
{-# INLINE allAncestors #-}

firstCommon :: Ord a => [a] -> [a] -> Maybe a
firstCommon xs ys = go (S.fromList xs) ys where
  go set [] = Nothing
  go set (y:ys) = if S.member y set
    then Just y
    else go set ys
{-# INLINE firstCommon #-}

transfers :: OrbitMap -> T.Text -> T.Text -> Maybe Int
transfers map startChild endChild = do
  start <- M.lookup startChild map
  end <- M.lookup endChild map
  let startAncestors = allAncestors map start
      endAncestors = allAncestors map end
  common <- firstCommon startAncestors endAncestors
  commonFromStart <- elemIndex common startAncestors
  commonFromEnd <- elemIndex common endAncestors
  pure $ commonFromStart + commonFromEnd

problem1 :: OrbitMap -> IO ()
problem1 map = print $ sum $ totalOrbits map <$> M.keys map

problem2 :: OrbitMap -> IO ()
problem2 map = case transfers map "YOU" "SAN" of
  Just transfers -> print transfers
  Nothing -> TIO.hPutStrLn stderr "Impossible transfer."

main :: IO ()
main = do
  args <- getArgs
  let action = case args of
        ["1"] -> Just problem1
        ["2"] -> Just problem2
        _ -> Nothing
  case action of
    Just m -> do
      contents <- TIO.getContents
      let orbits = orbitMap <$> (traverse parseOrbit $ T.lines contents)
      case orbits of
        Just map -> m map
        _ -> TIO.hPutStrLn stderr "Invalid input."
    Nothing -> TIO.hPutStrLn stderr "Usage: orbit 1|2 <input.txt"
