{-# LANGUAGE DeriveFunctor, PatternSynonyms, ViewPatterns #-}

module Q (
    Q
  , pattern Empty
  , (<:)
  , pattern (:>)
  , append
  , front
  , take
  , fromList
) where

import Prelude hiding (take)

newtype Q a = Q { unQ :: ([a], [a]) }
  deriving (Show, Functor)

-- invariant: Q is either ([], []) or ((_:_), _), never ([], _:_)

pattern Empty = Q ([], [])
pattern x :> xs <- (take -> Just (x, xs))
  where
    x :> Q (f, r) = Q (x:f, r)

(<:) :: Q a -> a -> Q a
(<:) = flip append

{-# INLINE append #-}
append :: a -> Q a -> Q a
append x (Q ([], _)) = Q ([x], [])
append x (Q (f, r)) = Q (f, x:r)

{-# INLINE front #-}
front :: Q a -> Maybe a
front (Q ([], _)) = Nothing
front (Q (x:_, _)) = Just x

{-# INLINE take #-}
take :: Q a -> Maybe (a, Q a)
take (Q ([], [])) = Nothing
take (Q ([x], r)) = Just (x, Q (reverse r, []))
take (Q (x:f, r)) = Just (x, Q (f, r))

{-# INLINE fromList #-}
fromList :: [a] -> Q a
fromList xs = Q (xs, [])
