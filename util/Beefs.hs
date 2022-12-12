{-# LANGUAGE PatternSynonyms #-}

module Beefs
  ( seek
  , seekSteps
  ) where

import Q (pattern Empty, pattern (:<|))
import qualified Q
import qualified Data.Set as S

seek :: Ord a => a -> (a -> [a]) -> (a -> Bool) -> Maybe [a]
seek start moves goal = reverse <$> path where
  path = seek' (Q.singleton (start, [])) S.empty
  seek' Empty _ = Nothing
  seek' ((s, p) :<| ss) vis
    | goal s = Just (s:p)
    | S.member s vis = seek' ss vis
    | otherwise = seek' (Q.append ss $ (, (s:p)) <$> moves s) (S.insert s vis)

seekSteps :: Ord a => a -> (a -> [a]) -> (a -> Bool) -> Maybe Int
seekSteps start moves goal = seek' (Q.singleton (start, 0)) S.empty where
  seek' Empty _ = Nothing
  seek' ((s, n) :<| ss) vis
    | goal s = Just n
    | S.member s vis = seek' ss vis
    | otherwise = seek' (Q.append ss $ (, (n + 1)) <$> moves s) (S.insert s vis)
