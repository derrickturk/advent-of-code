{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isAsciiLower)
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

newtype Text8 = Text8 { getText8 :: T.Text }
  deriving (Eq, Show)

text8 :: String -> Maybe Text8
text8 s
  | length s == 8 && all isAsciiLower s = Just $ Text8 $ T.pack s
  | otherwise = Nothing

chunks :: Int -> Text8 -> [T.Text]
chunks n (Text8 t) = [T.take n (T.drop k t) | k <- [0..(8 - n)]]

hasStraight :: Text8 -> Bool
hasStraight = any increasing . chunks 3 where
  increasing t = let a = T.index t 0
                     b = T.index t 1
                     c = T.index t 2
                  in b == succ a && c == succ b

hasPairs :: Text8 -> Bool
hasPairs pass = let enumerate = zip [0::Int ..]
                    taggedPairs = enumerate (chunks 2 pass)
                    samePairs = filter same taggedPairs
                    same (_, t) = T.index t 0 == T.index t 1
                    hasNotOverlapping ((i, _):(rest@((j, _):_)))
                      | j > i + 1 = True
                      | otherwise = hasNotOverlapping rest
                    hasNotOverlapping _ = False
                 in hasNotOverlapping samePairs

naughtyChar :: Char -> Bool
naughtyChar 'i' = True
naughtyChar 'o' = True
naughtyChar 'l' = True
naughtyChar _ = False

valid :: Text8 -> Bool
valid pass = hasStraight pass && hasPairs pass
  && not (T.any naughtyChar $ getText8 pass)

incr :: Text8 -> Text8
incr (Text8 pass) = Text8 $ incr' 7 pass where
  incr' (-1) t = t
  incr' n t = case T.index t n of 
    'z' -> incr' (n - 1) (edit n 'a' t)
    c -> edit n (succ c) t
  edit n c t = let (pre, post) = T.splitAt n t
                in pre <> T.singleton c <> T.drop 1 post

incrValid :: Text8 -> Text8
incrValid pass = let next = incr pass
                  in if valid next then next else incrValid next

main :: IO ()
main = do
  [input] <- getArgs
  let Just password = text8 input
  TIO.putStrLn $ getText8 $ incrValid password 
