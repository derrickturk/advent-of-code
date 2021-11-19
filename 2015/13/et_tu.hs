{-# LANGUAGE OverloadedStrings #-}

import Data.Ord (comparing)
import Data.List (maximumBy, nub, permutations)
import Text.Read (readMaybe)
import Data.Char (isSpace)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Feelings = M.Map (T.Text, T.Text) Integer

bestFeelings :: Feelings -> [T.Text] -> [T.Text]
bestFeelings fs = maximumBy (comparing (netFeelings fs)) . permutations

(*!) :: Feelings -> (T.Text, T.Text) -> Integer
fs *! pair = M.findWithDefault 0 pair fs

netFeelings :: Feelings -> [T.Text] -> Integer
netFeelings _ [] = 0
netFeelings fs (x:xs) = go 0 x x xs where
  go total first final [] = total + fs *! (first, final) + fs *! (final, first)
  go total first prev (y:ys) =
    go (total + fs *! (y, prev) + fs *! (prev, y)) first y ys

parseFeelings :: T.Text -> Maybe ((T.Text, T.Text), Integer)
parseFeelings line = case T.split isSpace line of
  [ feeler
    , "would"
    , sign
    , units
    , "happiness"
    , "units"
    , "by"
    , "sitting"
    , "next"
    , "to"
    , whom
    ] -> do
      val <- (*) <$> parseSign sign <*> readMaybe (T.unpack units)
      let whom' = T.init whom
      pure ((feeler, whom'), val)
  _ -> Nothing
  where
    parseSign "gain" = Just 1
    parseSign "lose" = Just (-1)
    parseSign _ = Nothing

main :: IO ()
main = do
  Just feels <- traverse parseFeelings . T.lines <$> TIO.getContents
  let feelsMap = M.fromList feels
      peeps = nub $ fst . fst <$> feels
  print $ bestFeelings feelsMap peeps
  print $ netFeelings feelsMap $ bestFeelings feelsMap peeps
  print $ bestFeelings feelsMap ("SELF":peeps)
  print $ netFeelings feelsMap $ bestFeelings feelsMap ("SELF":peeps)
