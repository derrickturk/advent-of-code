{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Prufrock.Compiler (
    MonadGenSym(..)
  , Compile(..)
  , runCompile
  , compileIR
) where

import qualified Data.Text as T
import qualified Data.Sequence as S

import Control.Monad.State.Strict

import Prufrock.Analyze
import Prufrock.Asm (L(..))
import Prufrock.Grammar
import Prufrock.IR

-- not really a register...
data Reg
  = GlobalVar T.Text
  | LocalVar Int
  | Inline -- inline into "self-modifying code"
  deriving (Eq, Show)

class Monad m => MonadGenSym m where
  gensym :: m T.Text

class MonadGenSym m => MonadRegAlloc m where
  alloc :: m Reg
  free :: Reg -> m ()
  enterFunction :: Int -> m ()
  exitFunction :: m ()

{- TODO: implement this - the idea is to use circular buffers of
 -   "available registers" - we'll stick regs back on the end when they're
 -   freed and add additional regs from memory or locals as needed
 -}
data CompileState
  = CompileState { globalFree :: S.Seq T.Text
                 , localFree :: S.Seq Int
                 , nextLocal :: Maybe Int
                 , nextGenSym :: Int
                 } deriving (Eq, Show)

initCompileState :: CompileState
initCompileState = CompileState S.empty S.empty Nothing 0
{-# INLINE initCompileState #-}

newtype Compile a = Compile { getCompile :: State CompileState a }
  deriving (Functor, Applicative, Monad, MonadState CompileState)

instance MonadGenSym Compile where
  gensym = do
    st <- get
    let i = nextGenSym st
    put $ st { nextGenSym = i + 1 }
    pure $ "_L" <> T.pack (show i)

instance MonadRegAlloc Compile where
  -- TODO
  alloc = pure Inline
  free _ = pure ()
  enterFunction n = modify $ \s -> s { nextLocal = Just n }
  exitFunction = modify $ \s -> s { nextLocal = Nothing }

runCompile :: Compile a -> a
runCompile (Compile m) = evalState m initCompileState
{-# INLINE runCompile #-}

compileIR :: MonadRegAlloc m => SymbolTable -> Program -> m IRProgram
compileIR tab prog = do
  ir <- mconcat <$> traverse (compileItem tab) prog
  if needsStack tab
    then pure (ir <> IRProgram [] [] [Labeled "_Stack" (Value 0)])
    else pure ir
{-# INLINE compileIR #-}

compileItem :: MonadRegAlloc m => SymbolTable -> Item -> m IRProgram
compileItem tab (StmtItem s) = compileStmt tab s
compileItem tab (FnDefItem def) = compileFn tab def
{-# INLINE compileItem #-}

compileStmt :: MonadRegAlloc m => SymbolTable -> Stmt -> m IRProgram
compileStmt = undefined

compileFn :: MonadRegAlloc m => SymbolTable -> FnDef -> m IRProgram
compileFn = undefined
