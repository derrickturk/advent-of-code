{-# LANGUAGE OverloadedStrings, TupleSections, FlexibleInstances, GADTs #-}

import Data.Char
import Data.String
import Control.Applicative
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

newtype Parser a = Parser { runParser :: T.Text -> Either T.Text (T.Text, a) }

instance Functor Parser where
  fmap f (Parser p) = Parser ((fmap . fmap) f . p)

instance Applicative Parser where
  pf <*> px = Parser (\i -> case runParser pf i of
    Left e -> Left e
    Right (rest, f) -> (fmap . fmap) f $ runParser px rest)
  pure x = Parser $ Right . (, x)

instance Alternative Parser where
  empty = Parser $ const (Left "nothing")
  pa <|> pb = Parser (\i -> case runParser pa i of
    Left _ -> runParser pb i
    Right r -> Right r)

instance Monad Parser where
  px >>= f = Parser (\i -> case runParser px i of
    Left e -> Left e
    Right (rest, x) -> runParser (f x) rest)

-- dumb hack
instance (a ~ T.Text) => IsString (Parser a) where
  fromString = literal . T.pack

parse :: Parser a -> T.Text -> Either T.Text a
parse p t = case runParser p t of
  Left e -> Left e
  Right (_, x) -> Right x

opt :: Parser a -> Parser (Maybe a)
opt = (<|> pure Nothing) . fmap Just

literal :: T.Text -> Parser T.Text
literal s = Parser $ maybe (Left s) (Right . (, s)) . T.stripPrefix s

charsWhere :: T.Text -> (Char -> Bool) -> Parser T.Text
charsWhere name p = Parser (\i -> let (match, rest) = T.span p i
                                   in if T.null match
                                        then Left name
                                        else Right (rest, match))

ws :: Parser ()
ws = opt (charsWhere "white space" isSpace) >> pure ()

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  ws
  pure x

eof :: Parser ()
eof = Parser (\i -> if T.null i
                      then Right (i, ())
                      else Left "end of file")

-- now it's time for the real stuff

data Expr
  = Lit Integer
  | Add Expr Expr
  | Mul Expr Expr
  deriving Show

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

parens :: Parser a -> Parser a
parens p = lexeme "(" *> p <* lexeme ")"

nat :: Parser Integer
nat = read . T.unpack <$> charsWhere "digits" isDigit

-- for part 1

term1 :: Parser Expr
term1 = (Lit <$> lexeme nat) <|> parens expr1

addTerm1 :: Parser (Expr -> Expr)
addTerm1 = flip Add <$> (lexeme "+" *> term1)

mulTerm1 :: Parser (Expr -> Expr)
mulTerm1 = flip Mul <$> (lexeme "*" *> term1)

expr1 :: Parser Expr
expr1 = do
  t <- term1
  rest <- many (addTerm1 <|> mulTerm1)
  pure $ foldl' (flip ($)) t rest

prog1 :: Parser [Expr]
prog1 = many expr1 <* eof

-- for part 2

term2 :: Parser Expr
term2 = (Lit <$> lexeme nat) <|> parens expr2

factor2 :: Parser Expr
factor2 = foldl' Add <$> term2 <*> many (lexeme "+" *> term2)

expr2 :: Parser Expr
expr2 = foldl' Mul <$> factor2 <*> many (lexeme "*" *> factor2)

prog2 :: Parser [Expr]
prog2 = many expr2 <* eof

main :: IO ()
main = do
  input <- TIO.getContents
  case parse prog1 input of
    Left e -> TIO.putStrLn $ "expected: " <> e
    Right p1 -> print $ sum $ fmap eval p1
  case parse prog2 input of
    Left e -> TIO.putStrLn $ "expected: " <> e
    Right p2 -> do
      print $ sum $ fmap eval p2
