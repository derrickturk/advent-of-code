{-# LANGUAGE OverloadedStrings #-}

module Prufrock.Parser (
    ident
  , ty
  , expr
  , stmt
) where

import Data.Void (Void)
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE

import Text.Megaparsec
import Text.Megaparsec.Char
-- import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

import Prufrock.Grammar

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

comma :: Parser Char
comma = lexeme $ char ','

term :: Parser Char
term = lexeme $ char ';'

enclosed :: T.Text -> T.Text -> Parser a -> Parser a
enclosed left right = between (symbol left) (symbol right)

integer :: Parser Int64
integer = lexeme $ L.signed space L.decimal

ident :: Parser Ident
ident = lexeme $ do
  var <- T.pack <$> ((:) <$> identBegin <*> identRest)
  case mkIdent var of
    Nothing -> failure
      (Just $ Label $ NE.fromList "invalid identifier")
      (S.singleton $ Label $ NE.fromList "identifier")
    Just i -> pure i
  where
    identBegin :: Parser Char
    identBegin = char '_' <|> letterChar
    identRest :: Parser [Char]
    identRest = many (char '_' <|> alphaNumChar)

ty :: Parser Type
ty =  IntType <$ "int"
  <|> PtrType <$> (char '*' *> lexeme ty)
  <|> FnPtrType <$> ("fn" *> enclosed "(" ")" (sepBy ty comma))
                <*> (optional $ lexeme "->" *> ty)

-- TODO: operators (uggggggh)
expr :: Parser Expr
expr =  try (Lit <$> integer)
    <|> Var <$> ident 
-- vvv EVIL LEFT RECURSION vvv
-- expr =  try (FnCall <$> expr <*> enclosed "(" ")" (sepBy expr comma))

stmt :: Parser Stmt
stmt = stmt' <* term
{-# INLINE stmt #-}

stmt' :: Parser Stmt
stmt' =  try (Decl <$> ident
                   <*> (symbol ":" *> lexeme ty)
                   <*> (optional $ symbol "=" *> expr))
     <|> try (Assign <$> expr <*> (symbol "=" *> expr))
     <|> try (Input <$> ("input" *> space1 *> expr))
     <|> try (Output <$> ("output" *> space1 *> expr))
     <|> (Return <$> ("return" *> space1 *> expr))
