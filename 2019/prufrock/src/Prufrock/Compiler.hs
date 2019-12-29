{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Prufrock.Compiler (
    MonadGenSym(..)
  , Compile(..)
  , runCompile
  , compileIR
) where

import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Sequence as S
import Data.Maybe (fromJust)

import Control.Monad.State.Strict

import Prufrock.Analyze
import Prufrock.Asm (L(..))
import Prufrock.Grammar
import Prufrock.IR

-- not really a register...
data Reg
  = GlobalVar T.Text
  | LocalVar Int64
  | Inline -- inline into "self-modifying code"
  deriving (Eq, Show)

class Monad m => MonadGenSym m where
  gensym :: m T.Text

class MonadGenSym m => MonadRegAlloc m where
  alloc :: m Reg
  free :: Reg -> m ()
  enterFunction :: Int64 -> m ()
  exitFunction :: m ()
  frameSize :: m (Maybe Int64)

{- TODO: implement this - the idea is to use circular buffers of
 -   "available registers" - we'll stick regs back on the end when they're
 -   freed and add additional regs from memory or locals as needed
 -}
data CompileState
  = CompileState { globalFree :: S.Seq T.Text
                 , localFree :: S.Seq Int64
                 , nextLocal :: Maybe Int64
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
  frameSize = gets nextLocal

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
compileItem tab (StmtItem s) = compileStmt tab Nothing s
compileItem tab (FnDefItem def) = compileFn tab def
{-# INLINE compileItem #-}

compileStmt :: MonadRegAlloc m
            => SymbolTable
            -> Maybe Ident
            -> Stmt
            -> m IRProgram

compileStmt tab Nothing (Decl var _ Nothing) =
  pure $ IRProgram [] [] [Labeled (getIdent var) (Value 0)]
compileStmt tab Nothing (Decl var _ (Just (Lit n))) =
  pure $ IRProgram [] [] [Labeled (getIdent var) (Value n)]
compileStmt tab Nothing (Decl var ty (Just e)) =
  (<>) <$> compileStmt tab Nothing (Decl var ty Nothing)
       <*> compileStmt tab Nothing (Assign (Var var) e)

compileStmt tab (Just fn) (Decl var _ Nothing) = pure mempty
compileStmt tab (Just fn) (Decl var ty (Just e)) =
  compileStmt tab (Just fn) (Assign (Var var) e)

compileStmt tab s (Assign e

compileFn :: MonadRegAlloc m => SymbolTable -> FnDef -> m IRProgram
compileFn tab def@(FnDef fn _ _ body) = do
  enterFunction $ fromIntegral $ baseFrameSize tab def
  prog <- mconcat <$> traverse (compileStmt tab (Just fn)) body
  frame <- fromJust <$> frameSize
  exitFunction
  let prolog = (IRProgram [] [Labeled (getIdent fn) (GrowStack frame)] [])
      epilog = (IRProgram [] [Anon (Ret frame)] [])
  pure $ prolog <> prog <> epilog

identDst :: SymbolTable -> Maybe Ident -> Ident -> Dst
identDst tab s name = case fromJust (getSymbolInfo tab s name) of
  SymbolFn _ _ -> Global (getIdent name)
  SymbolGlobalVar _ -> Global (getIdent name)
  SymbolLocalVar n _ -> Stack $ fromIntegral n

exprDst :: MonadRegAlloc m => SymbolTable -> Maybe Ident -> Expr -> m Dst
exprDst tab s (Var name) = pure $ identDst tab s name
exprDst tab s (UnOpApp DeRef e)
