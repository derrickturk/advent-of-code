{-# LANGUAGE OverloadedStrings #-}

module ProgramInfo (
    ProgramInfo(..)
  , Parser
  , parseProgramInfo
  , parse
  , parseErrorPretty
) where

import Data.Void
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data ProgramInfo = ProgramInfo { programName :: T.Text
                               , programWeight :: Int
                               , programChildren :: [T.Text]
                               } deriving Show

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

integer :: Parser Int
integer = lexeme L.decimal

ident :: Parser T.Text
ident = T.pack <$> lexeme (some letterChar)

descendents :: Parser [T.Text]
descendents = fromMaybe [] <$> optional descList where
  descList = lexeme "->" *> sepBy ident (lexeme ",")

parseProgramInfo :: Parser ProgramInfo
parseProgramInfo =
  ProgramInfo <$> ident
              <*> between (lexeme "(") (lexeme ")") integer
              <*> descendents
