{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (guard)
import Data.Char (isLower)
import Data.List (foldl')
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T

import FemtoParsec

type Link = (T.Text, T.Text)

link :: Parser Link
link = do
  begin <- letters
  _ <- char '-'
  end <- letters
  pure (begin, end)

nodes :: [Link] -> S.Set T.Text
nodes = foldl' see S.empty where
  see s (from, to) = S.insert from $ S.insert to s

graph :: [Link] -> M.Map T.Text [T.Text]
graph = foldl' see M.empty where
  see m (from, to) = M.alter (update to) from $ M.alter (update from) to m
  update to Nothing = Just [to]
  update to (Just tos) = Just (to:tos)

compile :: [Link] -> (S.Set T.Text, M.Map T.Text [T.Text])
compile ls = (nodes ls, graph ls)

little :: T.Text -> Bool
little = T.all (isLower)

validPaths :: M.Map T.Text [T.Text] -> T.Text -> T.Text -> [[T.Text]]
validPaths m from to = validPaths' (S.singleton from) m from to where
  validPaths' seen m from to
    | from == to = [[]]
    | otherwise = do
        next <- M.findWithDefault [] from m
        guard $ not $ (little next && S.member next seen)
        (:) <$> pure next <*> validPaths' (S.insert next seen) m next to

validPaths2 :: M.Map T.Text [T.Text] -> T.Text -> T.Text -> [[T.Text]]
validPaths2 m from to = validPaths' (S.singleton from, Nothing) m from to where
  validPaths' (seen, special) m from to
    | from == to = [[]]
    | otherwise = do
        next <- M.findWithDefault [] from m
        guard $ not $ (next == "start" || next == "end") && S.member next seen
        if little next && S.member next seen
          then case special of
            Just _ -> []
            Nothing -> (:) <$> pure next
                           <*> validPaths' (seen, Just next) m next to
          else (:) <$> pure next
                   <*> validPaths' (S.insert next seen, special) m next to

main :: IO ()
main = do
  Just links <- parseStdin $ some $ lexeme link
  let (_, m) = compile links
  print $ length $ validPaths m "start" "end"
  print $ length $ validPaths2 m "start" "end"
