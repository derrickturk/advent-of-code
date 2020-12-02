{-# LANGUAGE OverloadedStrings #-}

import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Rule = Rule Int Int Char
  deriving (Eq, Show)

data Query = Query Rule T.Text
  deriving (Eq, Show)

parseQuery :: T.Text -> Maybe Query
parseQuery line = case T.splitOn " " line of
  [range, char, pass] -> Query <$> parseRule range char <*> pure pass
  _ -> Nothing

parseRule :: T.Text -> T.Text -> Maybe Rule
parseRule range char = case T.splitOn "-" range of
  [low, high] -> Rule <$> readMaybe (T.unpack low)
                      <*> readMaybe (T.unpack high)
                      <*> pure (T.head char)
  _ -> Nothing

valid :: Query -> Bool
valid (Query (Rule low high char) pass) =
  let n = T.count (T.singleton char) pass
   in low <= n && high >= n

main :: IO ()
main = do
  queries <- sequence . fmap parseQuery . T.lines <$> TIO.getContents
  case queries of
    Just queries' -> print $ length $ filter valid queries'
    Nothing -> putStrLn "invalid query"
