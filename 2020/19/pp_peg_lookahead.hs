{-# LANGUAGE OverloadedStrings, TupleSections, FlexibleInstances, GADTs #-}

import Data.Char
import Data.String
import Control.Applicative
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Debug.Trace

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

validate :: Parser a -> T.Text -> Bool
validate p t = case runParser p t of
  Left _ -> False
  Right _ -> True

lookahead :: Parser a -> Parser ()
lookahead p = Parser (\i -> case runParser p i of
  Left e -> Left e
  Right _ -> Right (i, ()))

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

ws' :: Parser ()
ws' = opt (charsWhere "white space, but not newline"
                     (\c -> isSpace c && c /= '\n'))
   >> pure ()

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  ws
  pure x

-- newlines not allowed
lexeme' :: Parser a -> Parser a
lexeme' p = do
  x <- p
  ws'
  pure x

eof :: Parser ()
eof = Parser (\i -> if T.null i
                      then Right (i, ())
                      else Left "end of file")

-- now it's time for the real stuff

data Rule
  = RuleRef Int
  | Chain Rule Rule
  | Alt Rule Rule
  | Lit T.Text
  | Lookahead Rule
  deriving Show

type RuleSet = M.Map Int Rule 
type ParserSet = M.Map Int (Parser ())

loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

compile :: RuleSet -> ParserSet
compile set = loeb (compileRule <$> set)

compileRule :: Rule -> ParserSet -> Parser ()
compileRule (RuleRef n) set = case set M.!? n of
  Just p -> p
  _ -> empty
compileRule (Chain r1 r2) set = compileRule r1 set >> compileRule r2 set
compileRule (Alt r1 r2) set = compileRule r1 set <|> compileRule r2 set
compileRule (Lit t) _ = literal t *> pure ()
compileRule (Lookahead r) set = lookahead $ compileRule r set

nat :: Parser Int
nat = read . T.unpack <$> charsWhere "digits" isDigit

atomicRule :: Parser Rule
atomicRule =  lexeme' $
      (RuleRef <$> nat)
  <|> (Lit <$> ("\"" *> (charsWhere "literal chars" (/= '"')) <* "\""))
  <|> (Lookahead <$> (lexeme "?(" *> rule <* lexeme ")"))

chainRule :: Parser Rule
chainRule = do
  r <- atomicRule
  rs <- many atomicRule
  pure $ foldl' Chain r rs

rule :: Parser Rule
rule = do
  r <- chainRule
  rs <- many $ lexeme' "|" *> chainRule
  pure $ foldl' Alt r rs

numberedRule :: Parser (Int, Rule)
numberedRule = (,) <$> nat <*> (lexeme' ":" *> rule)

ruleSet :: Parser RuleSet
ruleSet = M.fromList <$> some (numberedRule <* ws)

{- for part 2, replace 8 with:
 - 8: 42 ?(11) | 42 8
 - and 11 with:
 - 11: 42 31 | 42 11 31
 -}

main :: IO ()
main = do
  input <- TIO.getContents
  case runParser ruleSet input of
    Left e -> TIO.putStrLn $ "expected: " <> e
    Right (rest, set) -> case compile set M.!? 0 of
      Nothing -> TIO.putStrLn "invalid ruleset"
      Just p -> do
        let lines = T.lines rest
        print $ length $ filter (validate $ p *> eof) $ lines
