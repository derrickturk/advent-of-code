{-# LANGUAGE OverloadedStrings #-}

module Parse (
    parse
  , parseErrorPretty
  , plant
  , plants
  , rule
  , setup
  , only
) where

import Data.Void
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad (replicateM)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Vector.Unboxed as V
import Plants

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

plant :: Parser Bool
plant = try (True <$ char '#') <|> (False <$ char '.')

plants :: Parser Plants
plants = Plants 0 . V.fromList <$> some plant

-- are these always 5?
rule :: Parser Rule
rule = Rule <$> (lexeme $ V.fromList <$> replicateM 5 plant)
            <*> (lexeme "=>" *> plant)

setup :: Parser (Plants, [Rule])
setup = (,) <$> (lexeme "initial state:" *> lexeme plants)
            <*> some (lexeme rule)

only :: Parser a -> Parser a
only = (<* eof)
