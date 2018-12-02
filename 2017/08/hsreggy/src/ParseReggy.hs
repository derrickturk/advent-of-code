{-# LANGUAGE OverloadedStrings #-}

module ParseReggy (
    Parser
  , parse
  , parseErrorPretty
  , reg
  , stmt
  , regOp
  , cond
  , cmpOp
  , reggy
) where

import Data.Void
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Reggy

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

integer :: Parser Int
integer = L.signed space $ lexeme L.decimal

reg :: Parser Reg
reg = Reg . T.pack <$> lexeme (some letterChar)

stmt :: Parser Stmt
stmt = lexeme $ Stmt <$> reg <*> regOp <*> integer <*> cond

regOp :: Parser RegOp
regOp = lexeme $ try (Inc <$ "inc") <|> (Dec <$ "dec")

cond :: Parser Cond
cond = Cond <$> (lexeme "if" *> reg) <*> cmpOp <*> integer

cmpOp :: Parser CmpOp
cmpOp = lexeme $  try (LtEq <$ "<=")
              <|> try (GtEq <$ ">=")
              <|> try (Lt <$ "<")
              <|> try (Gt <$ ">")
              <|> try (Eq <$ "==")
              <|> (NotEq <$ "!=")

reggy :: Parser Reggy
reggy = some stmt
