module GreatWall (
    modinv
  , crt
) where

-- solve a system of equations x = an (mod mn), represented as (a, m) pairs
crt :: [(Integer, Integer)] -> Maybe Integer
crt eqns = let moduliProduct = product $ snd <$> eqns
               partial (a, m) = do
                 let b = moduliProduct `div` m
                 inv <- modinv b m
                 pure $ a * b * inv
            in (`mod` moduliProduct) . sum <$> traverse partial eqns

{-# INLINE modinv #-}
modinv :: Integer -> Integer -> Maybe Integer
modinv u v = go 1 u 0 v False where
  go u1 u3 v1 v3 oddIter
    | v3 == 0 = if u3 /= 1
                  then Nothing
                  else if oddIter
                    then Just $ v - u1
                    else Just u1
    | otherwise = let q = u3 `div` v3
                      t3 = u3 `mod` v3
                      t1 = u1 + q * v1
                   in go v1 v3 t1 t3 (not oddIter)
