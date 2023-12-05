{-# LANGUAGE GADTs #-}

module FemtoParsec (
    Parser(..)
  , parse
  , string
  , optional
  , eof
  , only
  , charP
  , char
  , eol
  , chars
  , chars1
  , space
  , space1
  , digits
  , letters
  , lexeme
  , lexeme'
  , unsignedInteger
  , integer
  , unsignedIntNum
  , intNum
  , sepBy
  , sepBy'
  , parseStdin
  , many
  , some
  , (<|>)
  , empty
) where

import Data.String (IsString(..))
import Data.Char (isSpace, isDigit, isAlpha)
import Control.Monad (MonadPlus(..), guard)
import Control.Applicative (Alternative(..))
-- import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

newtype Parser a = Parser { runParser :: T.Text -> Maybe (a, T.Text) }

instance Functor Parser where
  fmap f p = Parser $ \t -> case runParser p t of
    Just (x, rest) -> Just (f x, rest)
    Nothing -> Nothing

instance Applicative Parser where
  pure x = Parser $ \t -> Just (x, t)
  pf <*> px = Parser $ \t -> do 
    (f, t') <- runParser pf t
    (x, t'') <- runParser px t'
    pure (f x, t'')

instance Monad Parser where
  pa >>= k = Parser $ \t -> do
    (a, t') <- runParser pa t
    runParser (k a) t'

instance Alternative Parser where
  empty = Parser $ const Nothing
  px <|> py = Parser $ \t ->
    case runParser px t of
      Nothing -> runParser py t
      result -> result

instance MonadPlus Parser where

instance MonadFail Parser where
  fail _ = mzero

instance (a ~ T.Text) => IsString (Parser a) where
    fromString = string . T.pack

{-# INLINE parse #-}
parse :: Parser a -> T.Text -> Maybe a
parse p t = fst <$> runParser p t

{-# INLINE string #-}
string :: T.Text -> Parser T.Text
string toMatch = Parser $ \t -> case T.stripPrefix toMatch t of
  Just suffix -> Just (toMatch, suffix)
  Nothing -> Nothing

{-# INLINE optional #-}
optional :: Parser a -> Parser (Maybe a)
optional = (<|> pure Nothing) . fmap Just

{-# INLINE eof #-}
eof :: Parser ()
eof = Parser $ \t -> if T.null t then Just ((), t) else Nothing

{-# INLINE only #-}
only :: Parser a -> Parser a
only = (<* eof)

{-# INLINE charP #-}
charP :: (Char -> Bool) -> Parser Char
charP p = Parser $ \t -> case T.uncons t of
  Just (c, rest) | p c -> Just (c, rest)
  _ -> Nothing

{-# INLINE char #-}
char :: Char -> Parser Char
char = charP . (==)

{-# INLINE eol #-}
eol :: Parser ()
eol = char '\n' >> pure ()

{-# INLINE chars #-}
chars :: (Char -> Bool) -> Parser T.Text
chars p = Parser $ \t -> Just (T.span p t)

{-# INLINE chars1 #-}
chars1 :: (Char -> Bool) -> Parser T.Text
chars1 p = do
  cs <- chars p
  guard $ not $ T.null cs
  pure cs

{-# INLINE space #-}
space :: Parser T.Text
space = chars isSpace

{-# INLINE space1 #-}
space1 :: Parser T.Text
space1 = chars1 isSpace

{-# INLINE digits #-}
digits :: Parser T.Text
digits = chars1 isDigit

{-# INLINE letters #-}
letters :: Parser T.Text
letters = chars1 isAlpha

{-# INLINE lexeme #-}
lexeme :: Parser a -> Parser a
lexeme = (<* space1)

-- optionally followed by space
{-# INLINE lexeme' #-}
lexeme' :: Parser a -> Parser a
lexeme' = (<* space)

{-# INLINE unsignedInteger #-}
unsignedInteger :: Parser Integer
unsignedInteger = read . T.unpack <$> digits

{-# INLINE integer #-}
integer :: Parser Integer
integer = ($) <$> sign <*> unsignedInteger

{-# INLINE unsignedIntNum #-}
unsignedIntNum :: Num a => Parser a
unsignedIntNum = fromIntegral <$> unsignedInteger 

{-# INLINE intNum #-}
intNum :: Num a => Parser a
intNum = fromIntegral <$> integer 

{-# INLINE sepBy #-}
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep p = (:) <$> p <*> many (sep >> p)

-- might be empty
{-# INLINE sepBy' #-}
sepBy' :: Parser a -> Parser b -> Parser [b]
sepBy' sep p = sepBy sep p <|> pure []

{-# INLINE sign #-}
sign :: Parser (Integer -> Integer)
sign = (id <$ char '+') <|> (negate <$ char '-') <|> pure id

{-# INLINE parseStdin #-}
parseStdin :: Parser a -> IO (Maybe a)
parseStdin p = parse (only p) <$> TIO.getContents
