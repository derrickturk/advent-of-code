{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Q
  ( Q
  , empty
  , singleton
  , peek
  , pop
  , push
  , fromList
  , toList
  , append
  , pattern Empty
  , pattern (:<|)
  , pattern (:|>)
  ) where

import Data.Foldable (foldl')
import Data.List (unfoldr)

data Q a = Q [a] [a]
  deriving Show

{-# INLINE empty #-}
empty :: Q a
empty = Q [] []

{-# INLINE singleton #-}
singleton :: a -> Q a
singleton x = Q [x] []

{-# INLINE peek #-}
peek :: Q a -> Maybe a
peek (Q [] []) = Nothing
peek (Q (x:_) _) = Just x
peek (Q [] ys) = Just $ last ys

pop :: Q a -> Maybe (a, Q a)
pop (Q [] []) = Nothing
pop (Q (x:xs) ys) = Just (x, Q xs ys)
pop (Q [] ys) = pop $ Q (reverse ys) []

{-# INLINE unpop #-}
unpop :: a -> Q a -> Q a
unpop x (Q xs ys) = Q (x:xs) ys

{-# INLINE push #-}
push :: a -> Q a -> Q a
push y (Q xs ys) = Q xs (y:ys)

unpush :: Q a -> Maybe (a, Q a)
unpush (Q [] []) = Nothing
unpush (Q xs (y:ys)) = Just (y, Q xs ys)
unpush (Q xs []) = unpush $ Q [] (reverse xs)

{-# INLINE fromList #-}
fromList :: [a] -> Q a
fromList xs = Q xs []

{-# INLINE toList #-}
toList :: Q a -> [a]
toList = unfoldr pop

{-# INLINE append #-}
append :: Foldable t => Q a -> t a -> Q a
append = foldl' (flip push)

instance Semigroup (Q a) where
  {-# INLINE (<>) #-}
  Q as bs <> Q cs ds = Q (as <> reverse bs <> cs <> reverse ds) []

instance Monoid (Q a) where
  {-# INLINE mempty #-}
  mempty = empty

instance Functor Q where
  {-# INLINE fmap #-}
  fmap f (Q xs ys) = Q (f <$> xs) (f <$> ys)

instance Foldable Q where
  {-# INLINE foldMap #-}
  foldMap f (Q xs ys) = foldMap f (xs <> reverse ys)

instance Traversable Q where
  traverse f q = case pop q of
    Just (x, q') -> push <$> f x <*> traverse f q'
    Nothing -> pure empty

pattern Empty :: Q a
pattern Empty = Q [] []

pattern (:<|) :: a -> Q a -> Q a
pattern x :<| xs <- (pop -> Just (x, xs)) where
  (:<|) = unpop

pattern (:|>) :: Q a -> a -> Q a
pattern ys :|> y <- (unpush -> Just (y, ys)) where
  (:|>) = flip push

{-# COMPLETE Empty, (:<|) #-}
{-# COMPLETE Empty, (:|>) #-}
