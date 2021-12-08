{-# LANGUAGE TypeApplications #-}

import FemtoParsec hiding (digits)

import Control.Monad (guard, replicateM)
import Data.List (sort, permutations)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import qualified Data.Text as T

digitSegments :: [(Int, String)]
digitSegments = [ (0, "abcefg")
                , (1, "cf")
                , (2, "acdeg")
                , (3, "acdfg")
                , (4, "bcdf")
                , (5, "abdfg")
                , (6, "abdefg")
                , (7, "acf")
                , (8, "abcdefg")
                , (9, "abcdfg")
                ]

segmentDigits :: [(String, Int)]
segmentDigits = swap <$> digitSegments

type Permuted = [(Char, Char)]

permuted :: [Permuted]
permuted = do
  p <- permutations "abcdefg"
  pure $ zip "abcdefg" p

translate :: Permuted -> String -> String
translate ps s = fromJust . (`lookup` ps) <$> s

example :: Parser ([String], [String])
example = do
  tests <- replicateM 10 $ lexeme segments
  _ <- lexeme $ char '|'
  digit <- replicateM 4 $ lexeme segments
  pure (T.unpack <$> tests, T.unpack <$> digit)
  where
    segments = chars1 (\c -> c >= 'a' && c <= 'g')

candidateDigits :: String -> [Int]
candidateDigits test = fst
  <$> filter ((== length test) . length . snd) digitSegments

translateDigits :: [String] -> Permuted -> Maybe [Int]
translateDigits ss ps =
  let xlated = translate ps <$> ss
   in traverse ((`lookup` segmentDigits) . sort) xlated

valid :: [String] -> Permuted -> Bool
valid ex ps =
  let digits = candidateDigits <$> ex
   in case translateDigits ex ps of
     Just digits' -> all (uncurry elem) $ zip digits' digits
     Nothing -> False

solve :: [String] -> [String] -> [Int]
solve exs outputs = do
  ps <- permuted
  guard $ valid exs ps
  fromJust $ translateDigits outputs ps

main :: IO ()
main = do
  Just exs <- parseStdin $ some example
  print $ length $ filter (known1478 . candidateDigits) $ concatMap snd exs
  print $ sum $ (read @Int . concat . fmap show . uncurry solve) <$> exs
  where
    known1478 [1] = True
    known1478 [4] = True
    known1478 [7] = True
    known1478 [8] = True
    known1478 _ = False
