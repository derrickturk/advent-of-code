{-# LANGUAGE OverloadedStrings #-}

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

main :: IO ()
main = do
  Just codes <- parseStdin $ many $ lexeme code
  mapM_ TIO.putStrLn codes
  print $ sum $ T.length <$> codes
