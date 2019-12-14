{-# LANGUAGE DeriveFunctor #-}

module IntCodeC.Asm (
    ReadOperand(..)
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

data ReadOperand
  = Immediate Int64
  | Memory WriteOperand
  deriving (Eq, Show)

data WriteOperand
  = Position Word64
  | Relative Int64
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
