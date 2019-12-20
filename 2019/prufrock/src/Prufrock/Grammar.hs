{-# LANGUAGE OverloadedStrings #-}

module Prufrock.Grammar (
    keywords
  , Ident
  , getIdent
  , mkIdent
  , unsafeIdent
  , Type(..)
  , Expr(..)
  , UnaryOp(..)
  , BinaryOp(..)
  , Stmt(..)
  , FnDef(..)
  , Item(..)
  , Program
) where

import Data.Int (Int64)
import Data.Char (isAlpha, isAlphaNum, isUpper)
import qualified Data.Text as T

keywords :: [T.Text]
keywords = [ "int"
           , "if"
           , "else"
           , "for"
           , "while"
           , "break"
           , "continue"
           , "fn"
           , "return"
           , "input"
           , "output"
           ]

newtype Ident = Ident { getIdent :: T.Text }
  deriving (Eq, Show, Ord)

data Type
  = IntType
  | PtrType Type
  | FnType [Type] Type
  | UnitType
  deriving (Eq, Show)

-- x | 73 | ...
data Expr
  = Var Ident
  | Lit Int64
  | FnCall Expr [Expr]
  | UnOpApp UnaryOp Expr
  | BinOpApp BinaryOp Expr Expr
  deriving (Eq, Show)

data UnaryOp
  = AddressOf
  | DeRef
  | Negate
  deriving (Eq, Show)

data BinaryOp
  = Add
  | Mul
  | Less
  | Eql
  | LessEql
  | LogAnd
  | LogOr
  deriving (Eq, Show)

data Stmt
  = Decl Ident Type (Maybe Expr)
  | Assign Expr Expr
  | AssignOp BinaryOp Expr Expr
  | Input Expr
  | Output Expr
  | Return (Maybe Expr)
  | Exit
  | ExprStmt Expr
  deriving (Eq, Show)

data FnDef = FnDef Ident [(Ident, Type)] Type [Stmt]
  deriving (Eq, Show)

data Item
  = StmtItem Stmt
  | FnDefItem FnDef
  deriving (Eq, Show)

type Program = [Item]

mkIdent :: T.Text -> Maybe Ident
mkIdent x = if valid then Just (Ident x) else Nothing where
  valid = not (T.null x) && validBegin (T.head x)
    && T.all validRest (T.tail x) && not reserved && not (elem x keywords)
  validBegin c = isAlpha c || c == '_'
  validRest c = isAlphaNum c || c == '_'
  reserved = T.head x == '_' && not (T.null $ T.tail x)
    && isUpper (T.head $ T.tail x)

unsafeIdent :: T.Text -> Ident
unsafeIdent = Ident
