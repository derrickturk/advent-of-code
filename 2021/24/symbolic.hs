{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeApplications #-}

import Prelude hiding (read)
import Control.Monad (forM_)

import Monad
import FemtoParsec

data Sym
  = SInput Int
  | SAdd Sym Sym
  | SMul Sym Sym
  | SDiv Sym Sym
  | SMod Sym Sym
  | SEql Sym Sym
  | SNotEql Sym Sym
  | SLit Int
  | SPrevZ Int
  deriving (Eq, Show)

instance Compute Sym (Int, [Sym]) where
  boot = CPU (SLit 0) (SLit 0) (SLit 0) (SLit 0)

  liftConst = SLit

  step instr s = Just $ step' instr s where
    step' (Input v) (State c (i, defs)) =
      let prevZ = read (Mem Z) c
          c' = (write (SInput i) v c)
       in if i == 0
            then State c' (i + 1, defs)
            else State (write (SPrevZ $ i - 1) Z c') (i + 1, defs <> [prevZ])
    step' (Add v o) (State c i) = State (binOp' SAdd v o c) i
    step' (Mul v o) (State c i) = State (binOp' SMul v o c) i
    step' (Div v o) (State c i) = State (binOp' SDiv v o c) i
    step' (Mod v o) (State c i) = State (binOp' SMod v o c) i
    step' (Eql v o) (State c i) = State (binOp' SEql v o c) i

    binOp' f v o c =
      let c' = binOp f v o c
       in write (simplify $ read (Mem v) c') v c'

bounds :: Sym -> Maybe (Int, Int)
bounds (SLit n) = Just (n, n)
bounds (SAdd s1 s2) = do
  (l1, h1) <- bounds s1
  (l2, h2) <- bounds s2
  pure (l1 + l2, h1 + h2)
bounds (SMul s1 s2) = do
  (l1, h1) <- bounds s1
  (l2, h2) <- bounds s2
  pure (l1 * l2, h1 * h2)
bounds (SInput _) = Just (0, 9)
bounds (SEql _ _) = Just (0, 1)
bounds (SNotEql _ _) = Just (0, 1)
bounds _ = Nothing

simplify :: Sym -> Sym
simplify (SAdd s1 s2) = case (simplify s1, simplify s2) of
  (SLit 0, s) -> s
  (s, SLit 0) -> s
  (SLit a, SLit b) -> SLit $ a + b
  (SAdd s (SLit a), SLit b) -> SAdd s (SLit $ a + b)
  (s1', s2') -> SAdd s1' s2'

simplify (SMul s1 s2) = case (simplify s1, simplify s2) of
  (SLit 0, _) -> SLit 0
  (_, SLit 0) -> SLit 0
  (SLit 1, s) -> s
  (s, SLit 1) -> s
  (SLit a, SLit b) -> SLit $ a * b
  (SMul s (SLit a), SLit b) -> SMul s (SLit $ a * b)
  (s1', s2') -> SMul s1' s2'

simplify (SDiv (SLit a) (SLit b)) = SLit $ a `div` b
simplify (SDiv s1 s2) = case (simplify s1, simplify s2) of
  (SLit 0, _) -> SLit 0
  (s, SLit 1) -> s
  (SLit a, SLit b) -> SLit $ a `div` b
  (SDiv s (SLit a), SLit b) -> SDiv s (SLit $ a * b)
  (s1', s2') -> SDiv s1' s2'

simplify (SMod (SLit a) (SLit b)) = SLit $ a `div` b
simplify (SMod s1 s2) = case (simplify s1, simplify s2) of
  (SLit 0, _) -> SLit 0
  (SLit a, SLit b) -> SLit $ a `mod` b
  (s, SLit n) -> case bounds s of
    Just (lo, hi)
      | 0 <= lo && hi < n -> s
    _ -> SMod s $ SLit n
  (s1', s2') -> SMod s1' s2'

simplify (SEql s1 s2) = case (simplify s1, simplify s2) of
  (SLit a, SLit b) -> if a == b then SLit 1 else SLit 0
  (SEql a b, SLit 0) -> SNotEql a b
  (SLit 0, SEql a b) -> SNotEql a b
  (s1', s2') -> if s1' == s2' then SLit 1 else SEql s1' s2'

simplify s = s

pprint :: Sym -> String
pprint = pprint' (0::Int) where
  pprint' _ (SInput n) = "input[" <> show n <> "]"
  pprint' _ (SLit n) = show n
  pprint' prec (SAdd s1 s2)
    | prec > 1 = "(" <> pprint' 1 s1 <> " + " <> pprint' 1 s2 <> ")"
    | otherwise = pprint' 1 s1 <> " + " <> pprint' 1 s2
  pprint' prec (SMul s1 s2)
    | prec > 2 = "(" <> pprint' 2 s1 <> " * " <> pprint' 2 s2 <> ")"
    | otherwise = pprint' 2 s1 <> " * " <> pprint' 2 s2
  pprint' prec (SDiv s1 s2)
    | prec > 2 = "(" <> pprint' 2 s1 <> " / " <> pprint' 2 s2 <> ")"
    | otherwise = pprint' 2 s1 <> " / " <> pprint' 2 s2
  pprint' prec (SMod s1 s2)
    | prec > 2 = "(" <> pprint' 2 s1 <> " % " <> pprint' 2 s2 <> ")"
    | otherwise = pprint' 2 s1 <> " % " <> pprint' 2 s2
  pprint' prec (SEql s1 s2)
    | prec > 0 = "(" <> pprint' 0 s1 <> " == " <> pprint' 0 s2 <> ")"
    | otherwise = pprint' 0 s1 <> " == " <> pprint' 0 s2
  pprint' prec (SNotEql s1 s2)
    | prec > 0 = "(" <> pprint' 0 s1 <> " /= " <> pprint' 0 s2 <> ")"
    | otherwise = pprint' 0 s1 <> " /= " <> pprint' 0 s2
  pprint' _ (SPrevZ n) = "prevZ[" <> show n <> "]"

main :: IO ()
main = do
  Just prog <- parseStdin program
  let Just (State c (_, defs)) = run @Sym @_ prog (State boot (0, []))
  forM_ (zip [(0::Int)..] defs) $ \(i, d) -> do
    putStr $ "prevZ[" <> show i <> "] = "
    putStrLn $ pprint d
  putStr "w = "
  putStrLn $ pprint $ simplify $ w c
  putStr "x = "
  putStrLn $ pprint $ simplify $ x c
  putStr "y = "
  putStrLn $ pprint $ simplify $ y c
  putStr "z = "
  putStrLn $ pprint $ simplify $ z c
