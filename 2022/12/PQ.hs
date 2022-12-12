{-# LANGUAGE Strict, DeriveFunctor #-}
{- priority queue using a splay tree
 - from "Purely Functional Data Structures",
 - by Chris Okasaki
 -}

module PQ (
    PQ
  , insert
  , findMin
  , deleteMin
  , takeMin
  , delete
  , deleteWhere
  , takeFirstWhere
  , partition
  , toList
  , fromList
) where

import Data.List (foldl')

data PQ a
  = Empty
  | Tree a (PQ a) (PQ a)
  deriving (Show, Functor)

{-# INLINE empty #-}
empty :: PQ a
empty = Empty

{-# INLINE insert #-}
insert :: Ord a => a -> PQ a -> PQ a
insert x q = let (small, big) = partition x q
              in Tree x small big

{-# INLINE findMin #-}
findMin :: PQ a -> Maybe a
findMin Empty = Nothing
findMin (Tree x Empty _) = Just x
findMin (Tree _ a _) = findMin a

{-# INLINE deleteMin #-}
deleteMin :: PQ a -> PQ a
deleteMin Empty = Empty
deleteMin (Tree _ Empty b) = b
deleteMin (Tree y (Tree _ Empty b) c) = Tree y b c
deleteMin (Tree y (Tree x a b) c) = Tree x (deleteMin a) (Tree y b c)

{-# INLINE takeMin #-}
takeMin :: PQ a -> Maybe (a, PQ a)
takeMin Empty = Nothing
takeMin (Tree x Empty b) = Just (x, b)
takeMin (Tree y (Tree x Empty b) c) = Just (x, Tree y b c)
takeMin (Tree y (Tree x a b) c) =
  let Just (aMin, aRest) = takeMin a
   in Just (aMin, Tree x aRest (Tree y b c))

delete :: Eq a => a -> PQ a -> PQ a
delete _ Empty = Empty
delete x (Tree y a b)
  | x == y = case b of
      Empty -> a
      _ -> let Just (z, b') = takeMin b
            in Tree z a b'
  | otherwise = Tree y (delete x a) (delete x b)

deleteWhere :: (a -> Bool) -> PQ a -> PQ a
deleteWhere _ Empty = Empty
deleteWhere p (Tree x a b)
  | p x = case b of
      Empty -> a
      _ -> let Just (y, b') = takeMin b
            in Tree y a b'
  | otherwise = Tree x (deleteWhere p a) (deleteWhere p b)

takeFirstWhere :: (a -> Bool) -> PQ a -> Maybe (a, PQ a)
takeFirstWhere _ Empty = Nothing
takeFirstWhere p (Tree x a b)
  | p x = case b of
      Empty -> Just (x, a)
      _ -> let Just (y, b') = takeMin b
            in Just (x, Tree y a b')
  | otherwise = case takeFirstWhere p a of
      Just (y, a') -> Just (y, Tree x a' b)
      Nothing -> case takeFirstWhere p b of
        Just (y, b') -> Just (y, Tree x a b')
        Nothing -> Nothing

partition :: Ord a => a -> PQ a -> (PQ a, PQ a)
partition _ Empty = (Empty, Empty)
partition pivot t@(Tree x a b)
  | x <= pivot = case b of
      Empty -> (t, Empty)
      Tree y b1 b2 -> if y <= pivot
        then let (small, big) = partition pivot b2
              in (Tree y (Tree x a b1) small, big)
        else let (small, big) = partition pivot b1
              in (Tree x a small, Tree y big b2)
  | otherwise = case a of
      Empty -> (Empty, t)
      Tree y a1 a2 -> if y <= pivot
        then let (small, big) = partition pivot a2
              in (Tree y a1 small, Tree x big b)
        else let (small, big) = partition pivot a1
              in (small, Tree y big (Tree x a2 b))

{-# INLINE toList #-}
toList :: PQ a -> [a]
toList Empty = []
toList (Tree x a b) = toList a <> (x:toList b)

{-# INLINE fromList #-}
fromList :: Ord a => [a] -> PQ a
fromList = foldl' (flip insert) empty
