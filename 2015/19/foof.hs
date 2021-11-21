{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Ord (comparing)
import Data.List (find, nub, minimumBy)
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

greedySeek :: [Reaction] -> T.Text -> T.Text -> Int
greedySeek rs start target = go 0 start target where
  go n s t
    | s == t = n
    | otherwise = let steps = reactAll rs s
                      next = minimumBy (comparing T.length) steps
                   in go (n + 1) next target

main :: IO ()
main = do
  Just (reactions, initial) <- parseStdin problem
  print $ length $ nub $ reactAll reactions initial
  let backReactions = swap <$> reactions
  {- TL;DR
  case find (("e" `elem`) . snd) $ reactionChains backReactions initial of
    Just (n, _) -> print n
    Nothing -> putStrLn "couldn't make the e"
  -}
  print $ greedySeek backReactions initial "e"
