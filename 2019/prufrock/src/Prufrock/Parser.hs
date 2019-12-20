{-# LANGUAGE OverloadedStrings #-}

module Prufrock.Parser (
    ident
  , ty
  , expr
  , stmt
  , item
  , program
) where

import Data.Void (Void)
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

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

expr :: Parser Expr
expr = makeExprParser nonRecExpr opTable where
  opTable = [ [ someFnCalls
              ] 
            , [ somePrefix "-" $ UnOpApp Negate
              , somePrefix "*" $ UnOpApp DeRef
              , prefix "&" $ UnOpApp AddressOf
              ]
            , [ binary "*" $ BinOpApp Mul
              , binary "+" $ BinOpApp Add
              ]
            , [ binary "<=" $ BinOpApp LessEql
              , binaryAmb "<" $ BinOpApp Less
              ]
            , [ binary "==" $ BinOpApp Eql
              ]
            , [ binary "&&" $ BinOpApp LogAnd
              ]
            , [ binary "||" $ BinOpApp LogOr
              ]
            ]
  fncall = do
    args <- enclosed "(" ")" (sepBy expr comma)
    pure $ \e -> FnCall e args
  someFnCalls = Postfix $ foldr1 (.) <$> some fncall
  prefix sym f = Prefix (f <$ symbol sym)
  -- postfix sym f = Postfix (f <$ symbol sym)
  binary sym f = InfixL (f <$ symbol sym)
  binaryAmb sym f = InfixL
    (f <$ (lexeme $ try $ string sym <* notFollowedBy punctuationChar))
  somePrefix sym f = Prefix $ foldr1 (.) <$> some (f <$ symbol sym)
  -- somePostfix sym f :: Prefix $ foldr1 (.) <$> some (f <$ symbol sym)

nonRecExpr :: Parser Expr
nonRecExpr =  try (Lit <$> integer)
          <|> Var <$> ident 
          <|> enclosed "(" ")" expr

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

fndef :: Parser FnDef
fndef = FnDef <$> (symbol "fn" *> ident)
              <*> (enclosed "(" ")" $ sepBy fnarg comma)
              <*> (optional $ symbol "->" *> lexeme ty)
              <*> (enclosed "{" "}" $ many stmt)
  where fnarg = (,) <$> ident <*> (symbol ":" *> lexeme ty)

item :: Parser Item
item = try (StmtItem <$> stmt) <|> (FnDefItem <$> fndef)
{-# INLINE item #-}

program :: Parser Program
program = space *> many item <* eof
{-# INLINE program #-}
