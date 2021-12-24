{-# LANGUAGE MultiParamTypeClasses, TypeApplications #-}

import Prelude hiding (read)

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
  deriving (Eq, Show)

instance Compute Sym Int where
  boot = CPU (SLit 0) (SLit 0) (SLit 0) (SLit 0)

  liftConst = SLit

  step instr s = Just $ step' instr s where
    step' (Input v) (State c i) = State (write (SInput i) v c) (i + 1)
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

main :: IO ()
main = do
  Just prog <- parseStdin program
  let Just (State c _) = run @Sym @Int prog (State boot 0)
  putStr "w = "
  print $ simplify $ w c
  putStr "x = "
  print $ simplify $ x c
  putStr "y = "
  print $ simplify $ y c
  putStr "z = "
  print $ simplify $ z c
