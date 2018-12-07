{-# LANGUAGE OverloadedStrings #-}

module Parse (
    Parser
  , parse
  , parseErrorPretty
  , node
  , edge
  , graph
) where

import Data.Void
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

import Kahn

type Parser = Parsec Void T.Text

node :: Parser Node
node = Node <$> upperChar

edge :: Parser Edge
edge = Edge <$> ("Step " *> node <* " must be finished before step ")
            <*> (node <* " can begin.")

graph :: Parser Graph
graph = many (edge <* space)
