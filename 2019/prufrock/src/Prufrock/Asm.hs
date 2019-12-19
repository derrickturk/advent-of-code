{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}

module Prufrock.Asm (
    W(..)
  , ReadOperand(..)
  , WriteOperand(..)
  , L(..)
  , Instruction(..)
  , Stmt(..)
  , Emit(..)
) where

import qualified Data.Text as T
import Data.Int (Int64)
import System.IO (Handle)
import qualified Data.Text.IO as TIO

data W -- "W"ord
  = Num Int64 
  | Lbl T.Text
  deriving (Eq, Show)

data ReadOperand
  = Imm W
  | Mem WriteOperand
  deriving (Eq, Show)

data WriteOperand
  = Abs W
  | Rel Int64
  deriving (Eq, Show)

data L a -- "L"abeled
  = Labeled T.Text a
  | Anon a
  deriving (Eq, Show, Functor)

data Instruction
  = Add (L ReadOperand) (L ReadOperand) (L WriteOperand)
  | Mul (L ReadOperand) (L ReadOperand) (L WriteOperand)
  | Input (L WriteOperand)
  | Output (L ReadOperand)
  | JmpTrue (L ReadOperand) (L ReadOperand)
  | JmpFalse (L ReadOperand) (L ReadOperand)
  | Less (L ReadOperand) (L ReadOperand) (L WriteOperand)
  | Eql (L ReadOperand) (L ReadOperand) (L WriteOperand)
  | AdjustRelBase (L ReadOperand)
  | Halt
  deriving (Eq, Show)

data Stmt
  = Instr Instruction
  | Value Int64
  deriving (Eq, Show)

class Emit a where
  emit :: a -> T.Text

  hEmit :: Handle -> a -> IO ()
  hEmit _ = TIO.putStr . emit

instance Emit W where
  emit (Num n) = T.pack $ show n
  emit (Lbl l) = l

instance Emit ReadOperand where
  emit (Imm n) = T.cons '$' (emit n)
  emit (Mem wop) = emit wop

instance Emit WriteOperand where
  emit (Abs ptr) = emit ptr
  emit (Rel n) = T.cons '(' $ T.snoc (T.pack $ show n) ')' 

instance Emit a => Emit (L a) where
  emit (Labeled lbl a) = lbl <> ": " <> emit a
  emit (Anon a) = emit a

instance Emit Instruction where
  emit (Add src1 src2 dst) = "add " <> emit src1 <> ", "
    <> emit src2 <> ", " <> emit dst
  emit (Mul src1 src2 dst) = "mul " <> emit src1 <> ", "
    <> emit src2 <> ", " <> emit dst
  emit (Input dst) = "in " <> emit dst
  emit (Output src) = "out " <> emit src
  emit (JmpTrue cnd dst) = "jnz " <> emit cnd <> ", " <> emit dst
  emit (JmpFalse cnd dst) = "jz " <> emit cnd <> ", " <> emit dst
  emit (Less src1 src2 dst) = "lt " <> emit src1 <> ", "
    <> emit src2 <> ", " <> emit dst
  emit (Eql src1 src2 dst) = "eq " <> emit src1 <> ", "
    <> emit src2 <> ", " <> emit dst
  emit (AdjustRelBase offset) = "rel " <> emit offset
  emit Halt = "hlt"

instance Emit Stmt where
  emit (Instr i) = emit i
  emit (Value v) = T.cons '$' $ T.pack $ show v

instance Emit a => Emit [a] where
  emit = mconcat . fmap (flip T.snoc '\n' . emit)
  hEmit h = mapM_ (TIO.hPutStrLn h . emit)
