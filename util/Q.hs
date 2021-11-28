module Q (
    Q
  , empty
  , singleton
  , head
  , tail
  , dequeue
  , enqueue
  , enqueueList
  , fromList
  , toList
) where

{- a queue after Okasaki
 - the invariant is that if the front is empty, so is the rear;
 - i.e. we never have Q ([], _:_)
 -}

import Prelude hiding (head, tail)
import Data.List (foldl')

newtype Q a = Q { unQ :: ([a], [a]) }
  deriving Show

{-# INLINE empty #-}
empty :: Q a
empty = Q ([], [])

{-# INLINE singleton #-}
singleton :: a -> Q a
singleton x = Q ([x], [])

{-# INLINE head #-}
head :: Q a -> Maybe a
head (Q ([], _)) = Nothing
head (Q (x:_, _)) = Just x

{-# INLINE tail #-}
tail :: Q a -> Maybe (Q a)
tail (Q ([], _)) = Nothing
tail (Q ([_], r)) = Just $ Q (reverse r, [])
tail (Q (_:f, r)) = Just $ Q (f, r)

{-# INLINE dequeue #-}
dequeue :: Q a -> Maybe (a, Q a)
dequeue (Q ([], _)) = Nothing
dequeue (Q ([x], r)) = Just (x, Q (reverse r, []))
dequeue (Q (x:f, r)) = Just (x, Q (f, r))

{-# INLINE enqueue #-}
enqueue :: a -> Q a -> Q a
enqueue x (Q ([], _)) = Q ([x], [])
enqueue x (Q (f, r)) = Q (f, x:r)

{-# INLINE enqueueList #-}
enqueueList :: [a] -> Q a -> Q a
enqueueList xs q = foldl' (flip enqueue) q xs

{-# INLINE fromList #-}
fromList :: [a] -> Q a
fromList xs = Q (xs, [])

{-# INLINE toList #-}
toList :: Q a -> [a]
toList (Q (f, r)) = f <> reverse r
