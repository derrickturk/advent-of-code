{-# LANGUAGE OverloadedStrings #-}

module Parse (
    Parser
  , parse
  , parseErrorPretty
  , group
  , only
) where

import Data.Void
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char

import Stream

type Parser = Parsec Void T.Text

group :: Parser Group
group = do
  char '{'
  bits <- sepBy (try (Left <$> garbage) <|> (Right <$> group)) (char ',')
  char '}'
  pure $ Group bits

-- drop chars that part 2 doesn't want counted
garbage :: Parser T.Text
garbage = do
  char '<'
  g <- T.concat <$> many garbageChar
  char '>'
  pure g
  where
    garbageChar = do
      c <- notChar '>'
      if c == '!'
        then anyChar *> pure T.empty
        else pure $ T.singleton c

only :: Parser a -> Parser a
only = (<* (space <* eof))
