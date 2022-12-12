{-# LANGUAGE PatternSynonyms #-}

module Beefs
  ( seek
  , seekN
  , seekSteps
  , seekStepsN
  ) where

import Q (pattern Empty, pattern (:<|))
import qualified Q
import qualified Data.Set as S

{-# INLINE seek #-}
seek :: Ord a => a -> (a -> [a]) -> (a -> Bool) -> Maybe [a]
seek start = seekN [start]

seekN :: Ord a => [a] -> (a -> [a]) -> (a -> Bool) -> Maybe [a]
seekN starts moves goal = reverse <$> path where
  path = seek' (Q.fromList $ (, []) <$> starts) S.empty
  seek' Empty _ = Nothing
  seek' ((s, p) :<| ss) vis
    | goal s = Just (s:p)
    | S.member s vis = seek' ss vis
    | otherwise = seek' (Q.append ss $ (, (s:p)) <$> moves s) (S.insert s vis)

{-# INLINE seekSteps #-}
seekSteps :: Ord a => a -> (a -> [a]) -> (a -> Bool) -> Maybe Int
seekSteps start = seekStepsN [start]

seekStepsN :: Ord a => [a] -> (a -> [a]) -> (a -> Bool) -> Maybe Int
seekStepsN starts moves goal = seek' (Q.fromList $ (, 0) <$> starts) S.empty
  where
  seek' Empty _ = Nothing
  seek' ((s, n) :<| ss) vis
    | goal s = Just n
    | S.member s vis = seek' ss vis
    | otherwise = seek' (Q.append ss $ (, (n + 1)) <$> moves s) (S.insert s vis)
