{-# LANGUAGE OverloadedStrings #-}

module Parse (
    Parser
  , parse
  , parseErrorPretty
  , rawLogEntry
  , rawLogEntries
  , logEntries
  , only
) where

import Data.Void
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Guard

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

integer :: Parser Int
integer = lexeme L.decimal

datetime :: Parser DateTime
datetime = DateTime <$> (char '[' *> integer) <*>
                        (char '-' *> integer) <*>
                        (char '-' *> integer) <*>
                        integer <*>
                        (char ':' *> integer <* lexeme (char ']'))

guardName :: Parser GuardId
guardName = lexeme "Guard #" *> (GuardId <$> integer)

rawAction :: Parser RawAction
rawAction =  try (RawBeginShift <$> guardName <* lexeme "begins shift")
         <|> try (RawSleep <$ lexeme "falls asleep")
         <|> (RawWake <$ lexeme "wakes up")

rawLogEntry :: Parser RawLogEntry
rawLogEntry = RawLogEntry <$> datetime <*> rawAction

rawLogEntries :: Parser [RawLogEntry]
rawLogEntries = some rawLogEntry

logEntries :: Parser (Maybe [LogEntry])
logEntries = cookLogEntries <$> rawLogEntries

only :: Parser a -> Parser a
only m = do
  r <- m
  eof
  pure r
