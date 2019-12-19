{-# LANGUAGE OverloadedStrings #-}

module Prufrock.Grammar (
    Ident
  , getIdent
  , ident
  , unsafeIdent
) where

import Data.Int (Int64)
import Data.Char (isAlpha, isAlphaNum, isUpper)
import qualified Data.Text as T

newtype Ident = Ident { getIdent :: T.Text }

{-
-- x | 73 | ...
data Expr
  = Var Ident
  | Lit Int64
  deriving (Eq, Show)
-}

ident :: T.Text -> Maybe Ident
ident x = if valid x then Just (Ident x) else Nothing where
  valid x = not (T.null x) && validBegin (T.head x)
    && T.all validRest (T.tail x) && not (reserved x)
  validBegin c = isAlpha c || c == '_'
  validRest c = isAlphaNum c || c == '_'
  reserved x = T.head x == '_' && not (T.null $ T.tail x)
    && isUpper (T.head $ T.tail x)

unsafeIdent :: T.Text -> Ident
unsafeIdent = Ident
