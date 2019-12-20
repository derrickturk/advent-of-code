{-# LANGUAGE OverloadedStrings #-}

module Prufrock.IR (
    -- MonadIR(..)
    Src(..)
  , Dst(..)
  , FnCall(..)
  , IRInstruction(..)
) where

import Data.Int (Int64)
import qualified Data.Text as T

{- TODO: where does AST -> IR code live?
import Control.Monad.State.Strict

class Monad m => MonadIR m where
  gensym :: m T.Text
-}

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
  = Store Src Dst
  | AddInto Src Src Dst
  | MulInto Src Src Dst
  | InputInto Dst
  | OutputFrom Src
  | Call FnCall [Src]
  | Ret
  deriving (Eq, Show)
