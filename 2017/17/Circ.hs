{-# LANGUAGE DeriveFunctor #-}

module Circ (
    Circ
  , empty
  , singleton
  , length
  , null
  , delete
  , insert
  , rotateR
  , rotateL
  , rotateNR
  , rotateNL
  , fromList
  , toList
  , focus
  , rotateTo
) where

import Prelude hiding (length, null)
import qualified Prelude as P

data Circ a
  = Empty
  | Circ a [a] [a]
  deriving (Show, Eq, Ord, Functor)

{-# INLINE empty #-}
empty :: Circ a
empty = Empty

{-# INLINE singleton #-}
singleton :: a -> Circ a
singleton x = Circ x [] []

{-# INLINE length #-}
length :: Circ a -> Int
length Empty = 0
length (Circ _ ls rs) = 1 + P.length ls + P.length rs

{-# INLINE null #-}
null :: Circ a -> Bool
null Empty = True
null _ = False

-- focus moves to the right
{-# INLINE delete #-}
delete :: Circ a -> Circ a
delete Empty = Empty
delete (Circ _ [] []) = Empty
delete (Circ _ ls (r0:rs)) = Circ r0 ls rs
delete (Circ x ls []) = delete (Circ x [] (reverse ls))

-- insert right of focus and move focus right (to new element) 
{-# INLINE insert #-}
insert :: a -> Circ a -> Circ a
insert x Empty = Circ x [] []
insert y (Circ x ls rs) = Circ y (x:ls) rs

rotateR :: Circ a -> Circ a
rotateR Empty = Empty
rotateR (Circ x [] []) = Circ x [] []
rotateR (Circ x ls (r0:rs)) = Circ r0 (x:ls) rs
rotateR (Circ x ls []) = rotateR (Circ x [] (reverse ls))

rotateL :: Circ a -> Circ a
rotateL Empty = Empty
rotateL (Circ x [] []) = Circ x [] []
rotateL (Circ x (l0:ls) rs) = Circ l0 ls (x:rs)
rotateL (Circ x [] rs) = rotateL (Circ x (reverse rs) [])

{-# INLINE rotateNR #-}
rotateNR :: Int -> Circ a -> Circ a
rotateNR n = (!! n) . iterate rotateR 

{-# INLINE rotateNL #-}
rotateNL :: Int -> Circ a -> Circ a
rotateNL n = (!! n) . iterate rotateL 

{-# INLINE fromList #-}
fromList :: [a] -> Circ a
fromList [] = Empty
fromList (x:xs) = Circ x [] xs

{-# INLINE toList #-}
toList :: Circ a -> [a]
toList Empty = []
toList (Circ x l r) = (x:r) <> reverse l

{-# INLINE focus #-}
focus :: Circ a -> Maybe a
focus Empty = Nothing
focus (Circ x _ _) = Just x

rotateTo :: Eq a => a -> Circ a -> Maybe (Circ a)
rotateTo _ Empty = Nothing
rotateTo x c@(Circ y ls rs)
  | x == y = Just c
  | Just (pre, post) <- splitOn x (y:rs) = Just $ Circ x (revShift ls pre) post
  | Just (pre, post) <- splitOn x (y:ls) = Just $ Circ x post (revShift rs pre)
  | otherwise = Nothing

{-# INLINE splitOn #-}
splitOn :: Eq a => a -> [a] -> Maybe ([a], [a])
splitOn _ [] = Nothing
splitOn x (y:ys)
  | x == y = Just ([], ys)
  | otherwise = (\(pre, post) -> (y:pre, post)) <$> splitOn x ys

{-# INLINE revShift #-}
revShift :: [a] -> [a] -> [a]
revShift xs [] = xs
revShift xs (y:ys) = revShift (y:xs) ys
