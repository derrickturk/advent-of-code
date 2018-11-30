module Main where

import Data.Foldable (foldl')
import qualified Data.Text as T
import qualified Data.Set as S
import System.IO (isEOF)
import qualified Data.Text.IO as TIO

wordsUnique :: [T.Text] -> Bool
wordsUnique = go S.empty where
  go _ [] = True
  go seen (w:rest) = if w `S.member` seen
    then False
    else go (S.insert w seen) rest

phrases :: IO [[T.Text]]
phrases = do
  done <- isEOF
  if done
    then pure []
    else (:) <$> (T.words <$> TIO.getLine) <*> phrases

main :: IO ()
main = length . filter wordsUnique <$> phrases >>= print
