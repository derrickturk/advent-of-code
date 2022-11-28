module ModInv (
    modInv
) where

modInv :: Int -> Int -> Maybe Int
modInv u v =
  let (iter', u1', u3', v1', v3') = go 1 1 u 0 v
      go iter u1 u3 v1 v3
        | v3 == 0 = (iter, u1, u3, v1, v3)
        | otherwise = let q = u3 `div` v3
                          t3 = u3 `mod` v3
                          t1 = u1 + q * v1
                       in go (-iter) v1 v3 t1 t3
   in if u3' /= 1
        then Nothing
        else Just $ if iter' < 0
                      then v - u1'
                      else u1'
