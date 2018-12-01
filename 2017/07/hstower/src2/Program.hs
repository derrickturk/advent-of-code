module Program (
    Program(..)
  , ParentMap
  , ChildMap
  , parentMap
  , root
  , childMap
  , toProgram
  , toRootProgram
  , weight
) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Foldable (foldl')
import ProgramInfo

-- this is surely not the most efficient way to do this

data Program = Program T.Text Int [Program]
  deriving Show

type ParentMap = M.Map T.Text T.Text
type ChildMap = M.Map T.Text (Int, [T.Text])

parentMap :: [ProgramInfo] -> ParentMap
parentMap = foldl' update M.empty where
  update m (ProgramInfo p _ cs) = foldl' (update1 p) m cs
  update1 p m c = M.insert c p m

root :: [ProgramInfo] -> ParentMap -> Maybe T.Text
root ps pmap = case filter (flip M.notMember pmap . programName) ps of
  [p] -> Just $ programName p
  _ -> Nothing

childMap :: [ProgramInfo] -> ChildMap
childMap = foldl' update M.empty where
  update m (ProgramInfo p w cs) = M.insert p (w, cs) m

toProgram :: ChildMap -> T.Text -> Maybe Program
toProgram cmap root = M.lookup root cmap >>= expand root where
  expand p (w, cs) = Program p w <$> traverse (toProgram cmap) cs

toRootProgram :: [ProgramInfo] -> Maybe Program
toRootProgram ps = root ps (parentMap ps) >>= toProgram (childMap ps)

weight :: Program -> Int
weight (Program _ w cs) = w + sum (fmap weight cs)
