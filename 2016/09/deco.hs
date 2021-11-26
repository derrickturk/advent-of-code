{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (Sum(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import FemtoParsec

nChars :: Int -> Parser T.Text
nChars n = Parser $
  \t -> let (pre, suf) = T.splitAt n t
         in if T.length pre == n
              then Just (pre, suf)
              else Nothing

repeater :: Parser T.Text
repeater = do
  _ <- "("
  n <- unsignedIntNum
  _ <- "x"
  m <- unsignedIntNum
  _ <- ")"
  what <- nChars n
  pure $ T.replicate m what

code :: Parser T.Text
code = mconcat <$> many (repeater <|> letters)

repeaterLen :: Parser (Sum Int)
repeaterLen = do
  _ <- "("
  n <- unsignedIntNum
  _ <- "x"
  m <- unsignedIntNum
  _ <- ")"
  what <- nChars n
  case runParser (only codeLen) what of
    Just (Sum x, _) -> pure (Sum (m * x))
    Nothing -> pure (Sum 0)

codeLen :: Parser (Sum Int)
codeLen = mconcat <$> many (repeaterLen <|> (Sum . T.length) <$> letters)

main :: IO ()
main = do
  input <- TIO.getContents
  let Just codes = parse (only $ many $ lexeme code) input
      Just lens = parse (only $ many $ lexeme codeLen) input
  mapM_ TIO.putStrLn codes
  print $ sum $ T.length <$> codes
  print $ sum lens
