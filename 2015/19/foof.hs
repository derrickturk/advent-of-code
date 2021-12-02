{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Ord (comparing)
import Data.List (nub, sortBy)
import Data.Tuple (swap)

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

reactAll :: [Reaction] -> T.Text -> [T.Text]
reactAll reactions start = reactions >>= \r -> react r start

reactionChains :: [Reaction] -> T.Text -> [(Int, [T.Text])]
reactionChains reactions start =
  zip [0..] $ iterate (concatMap (reactAll reactions)) [start]

greedySeek :: [Reaction] -> T.Text -> T.Text -> Maybe Int
greedySeek rs start target
  | start == target = Just 0
  | otherwise = let steps = sortBy (comparing T.length) $ reactAll rs start
                    go [] = Nothing
                    go (x:xs) = (+ 1) <$> (greedySeek rs x target <|> go xs)
                 in go steps

main :: IO ()
main = do
  Just (reactions, initial) <- parseStdin problem
  print $ length $ nub $ reactAll reactions initial
  let backReactions = swap <$> reactions
  print $ greedySeek backReactions initial "e"
