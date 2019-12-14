{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}

module IntCodeC.Asm (
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
import Data.Word (Word64)
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
  hEmit h = TIO.putStr . emit

instance Emit W where
  emit (Num n) = T.pack $ show n
  emit (Lbl l) = l

instance Emit ReadOperand where
  emit (Imm n) = T.cons '$' (emit n)
  emit (Mem wop) = emit wop

instance Emit WriteOperand where
  emit (Abs ptr) = emit ptr
  emit (Rel n) = T.cons '(' $ T.snoc (T.pack $ show n) ')' 
