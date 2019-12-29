{-# LANGUAGE OverloadedStrings #-}

module Prufrock.IR (
    Src(..)
  , Dst(..)
  , FnCall(..)
  , IRInstruction(..)
  , IRProgram(..)
) where

import Data.Int (Int64)
import qualified Data.Text as T

import Prufrock.Asm (L(..))

data Src
  = Const Int64 
  | LValue Dst
  deriving (Eq, Show)

data Dst
  = Global T.Text
  | Stack Int64
  deriving (Eq, Show)

data FnCall
  = CallGlobal T.Text
  | CallPtr Src
  deriving (Eq, Show)

data IRInstruction
  = Store (L Src) (L Dst)
  | AddInto (L Src) (L Src) (L Dst)
  | MulInto (L Src) (L Src) (L Dst)
  | InputInto (L Dst)
  | OutputFrom (L Src)
  | Call FnCall [Src]
  | Ret Int64
  | Value Int64
  | End
  deriving (Eq, Show)

data IRProgram = IRProgram { irText :: [L IRInstruction]
                           , irFunctions :: [L IRInstruction]
                           , irGlobals :: [L IRInstruction]
                           } deriving (Eq, Show)

instance Semigroup IRProgram where
  (IRProgram t1 f1 g1) <> (IRProgram t2 f2 g2) =
    IRProgram (t1 <> t2) (f1 <> f2) (g1 <> g2)

instance Monoid IRProgram where
  mempty = IRProgram [] [] []
