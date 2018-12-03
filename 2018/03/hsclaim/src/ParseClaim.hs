{-# LANGUAGE OverloadedStrings #-}

module ParseClaim (
    Parser
  , parse
  , parseErrorPretty
  , claim
  , claims
) where

import Data.Void
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Claim

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

integer :: Parser Int
integer = lexeme L.decimal

claim :: Parser Claim
claim = do
  char '#'
  id <- integer
  lexeme $ char '@'
  l <- integer
  char ','
  t <- integer
  lexeme $ char ':'
  w <- integer
  char 'x'
  h <- integer
  pure $ Claim id l (l + w - 1) t (t + h - 1)

claims :: Parser [Claim]
claims = some claim
