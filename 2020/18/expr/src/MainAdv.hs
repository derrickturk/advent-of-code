{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import Data.Void (Void)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.Megaparsec as MP
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

data Expr
  = Lit Integer
  | Mul Expr Expr
  | Add Expr Expr
  deriving (Eq, Show)

eval :: Expr -> Integer
eval (Lit n) = n
eval (Mul e1 e2) = eval e1 * eval e2
eval (Add e1 e2) = eval e1 + eval e2

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

nat :: Parser Integer
nat = lexeme L.decimal

term :: Parser Expr
term = parens expr <|> (Lit <$> nat) <?> "natural or (expression)"

expr :: Parser Expr
expr = makeExprParser term opTable

opTable :: [[Operator Parser Expr]]
opTable = [ [ InfixL (Add <$ symbol "+") ]
          , [ InfixL (Mul <$ symbol "*") ]
          ]

prog :: Parser [Expr]
prog = many expr

only :: Parser a -> Parser a
only = (<* eof)

main :: IO ()
main = do
  parsed <- parse (space *> only prog) "stdin" <$> TIO.getContents
  case parsed of
    Left e -> hPutStrLn stderr (errorBundlePretty e)
    Right prog -> print $ sum $ eval <$> prog
