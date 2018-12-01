module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M
import Data.Foldable (foldl')
import Data.Either (partitionEithers)
import ProgramInfo

type ParentMap = M.Map T.Text T.Text

parentMap :: [ProgramInfo] -> ParentMap
parentMap = foldl' update M.empty where
  update m (ProgramInfo p _ cs) = foldl' (update1 p) m cs
  update1 p m c = M.insert c p m

root :: [ProgramInfo] -> ParentMap -> Maybe T.Text
root ps pmap = case filter (flip M.notMember pmap . programName) ps of
  [p] -> Just $ programName p
  _ -> Nothing

main :: IO ()
main = do
  progs <- T.lines <$> TIO.getContents
  case partitionEithers ((parse parseProgramInfo "stdin") <$> progs) of
    ([], infos) -> case root infos (parentMap infos) of
      Just r -> TIO.putStr r
      Nothing -> putStrLn "no or multiple roots"
    (errs, _) -> mapM_ (putStr . parseErrorPretty) errs
