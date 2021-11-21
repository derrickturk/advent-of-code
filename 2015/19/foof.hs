{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.List (nub)

import FemtoParsec

type Reaction = (T.Text, T.Text)

reaction :: Parser Reaction
reaction = (,) <$> (lexeme letters <* lexeme "=>") <*> letters

problem :: Parser ([Reaction], T.Text)
problem = (,) <$> many (lexeme reaction) <*> lexeme letters

react :: Reaction -> T.Text -> [T.Text]
react (from, to) input = case T.breakOn from input of
  (_, "") -> []
  (pre, post) ->
    let replaced = pre <> to <> T.drop (T.length from) post
     in case T.uncons post of
          Just (c, post') -> replaced:((pre <>) . T.cons c <$> react (from, to) post')
          Nothing -> [replaced]

main :: IO ()
main = do
  Just (reactions, initial) <- parseStdin problem
  print $ length $ nub $ reactions >>= \r -> react r initial
