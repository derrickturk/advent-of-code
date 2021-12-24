{-# LANGUAGE MultiParamTypeClasses, TypeApplications #-}

import Monad
import FemtoParsec

data Sym
  = SInput Int
  | SAdd Sym Sym
  | SMul Sym Sym
  | SDiv Sym Sym
  | SMod Sym Sym
  | SEql Sym Sym
  | SLit Int
  deriving (Eq, Show)

instance Compute Sym Int where
  boot = CPU (SLit 0) (SLit 0) (SLit 0) (SLit 0)

  liftConst = SLit

  step instr s = Just $ step' instr s where
    step' (Input v) (State c i) = State (write (SInput i) v c) (i + 1)
    step' (Add v o) (State c i) = State (binOp SAdd v o c) i
    step' (Mul v o) (State c i) = State (binOp SMul v o c) i
    step' (Div v o) (State c i) = State (binOp SDiv v o c) i
    step' (Mod v o) (State c i) = State (binOp SMod v o c) i
    step' (Eql v o) (State c i) = State (binOp SEql v o c) i

main :: IO ()
main = do
  Just prog <- parseStdin program
  print $ run @Sym @Int prog (State boot 0)
