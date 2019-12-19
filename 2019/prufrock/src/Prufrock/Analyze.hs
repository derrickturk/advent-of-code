{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Prufrock.Analyze (
    AnalysisError(..)
  , FnEntry
  , FnTable
  , VarTable
  , SymbolTable(..)
  , symbolTable
) where

import qualified Data.Map.Strict as M

import Control.Monad.State.Strict
import Control.Monad.Except

import Prufrock.Grammar

data AnalysisError
  = AlreadyDefined Ident
  | Undefined Ident
  | TypeError Type Type
  deriving (Eq, Show)

data FnEntry = FnEntry [(Ident, Type)] (Maybe Type)
  deriving (Eq, Show)

type FnTable = M.Map Ident (FnEntry, VarTable)
type VarTable = M.Map Ident Type

data SymbolTable
  = SymbolTable { fnSymbols :: FnTable
                , globalSymbols :: VarTable
                } deriving (Eq, Show)

symbolTable :: Program -> Either AnalysisError SymbolTable
symbolTable = flip execStateT (SymbolTable M.empty M.empty) . mapM_ visitItem

-- TODO: look for vars in exprs - these can only refer back, not forward
visitItem :: (MonadState SymbolTable m, MonadError AnalysisError m)
          => Item
          -> m ()
visitItem (StmtItem stmt) = visitStmt Nothing stmt
visitItem (FnDefItem (FnDef name args ret body)) = do
  fns <- gets fnSymbols
  newFns <- M.alterF (\case
    Nothing -> pure $ Just (FnEntry args ret, M.empty)
    Just _ -> throwError $ AlreadyDefined name) name fns
  modify (\s -> s { fnSymbols = newFns })
  mapM_ (visitStmt (Just name)) body

visitStmt :: (MonadState SymbolTable m, MonadError AnalysisError m)
          => Maybe Ident
          -> Stmt
          -> m ()
visitStmt scope = undefined
