import FemtoParsec

import Control.Monad (replicateM)
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

main :: IO ()
main = do
  Just exs <- parseStdin $ some example
  print $ length $ filter (known1478 . candidateDigits) $ concatMap snd exs
  where
    known1478 [1] = True
    known1478 [4] = True
    known1478 [7] = True
    known1478 [8] = True
    known1478 _ = False
