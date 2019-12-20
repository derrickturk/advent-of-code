{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Prufrock.Compiler (
    MonadGenSym(..)
  , Compile(..)
  , runCompile
  , compileIR
) where

import qualified Data.Text as T

import Control.Monad.State.Strict

import Prufrock.Analyze
import Prufrock.Grammar
import Prufrock.IR

class Monad m => MonadGenSym m where
  gensym :: m T.Text

newtype Compile a = Compile { getCompile :: State Int a }
  deriving (Functor, Applicative, Monad, MonadState Int)

instance MonadGenSym Compile where
  gensym = do
    i <- get
    put $ i + 1
    pure $ "_L" <> T.pack (show i)

runCompile :: Compile a -> a
runCompile (Compile m) = evalState m 0
{-# INLINE runCompile #-}

compileIR :: MonadGenSym m => SymbolTable -> Program -> m [IRInstruction]
compileIR = undefined
