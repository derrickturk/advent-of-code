{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Prufrock.Analyze (
    AnalysisError(..)
  , FnTable
  , VarTable
  , SymbolTable
  , symbolTableFns
  , symbolTableGlobals
  , symbolTableScopes
  , getSymbolInfo
  , symbolTable
  , typecheck
) where

import qualified Data.Map.Strict as M

import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Except

import Prufrock.Grammar

data AnalysisError
  = AlreadyDefined Ident
  | Undefined Ident
  | TypeError Type Type
  | ExpectedFnType Type
  | ExpectedPtrType Type
  | NonConcreteType Type
  | NonPointableType Type -- are we going to have void*?
  | NonLValue Expr
  deriving (Eq, Show)

type FnTable = M.Map Ident ([(Ident, Type)], Type)
type VarTable = M.Map Ident Type

data SymbolTable
  = SymbolTable { symbolTableFns :: FnTable
                , symbolTableGlobals :: VarTable
                , symbolTableScopes :: M.Map Ident VarTable
                } deriving (Eq, Show)

data SymbolInfo
  = SymbolFn [(Ident, Type)] Type
  | SymbolVar Type
  deriving (Eq, Show)

getSymbolInfo :: SymbolTable -> Maybe Ident -> Ident -> Maybe SymbolInfo
getSymbolInfo (SymbolTable fns globs locs) s name
  | Just (args, ret) <- M.lookup name fns = Just $ SymbolFn args ret
  | otherwise = case s of
      Nothing -> SymbolVar <$> M.lookup name globs
      Just fn -> SymbolVar <$>
        (M.lookup name (locs M.! fn) <|> M.lookup name globs)

insertScope :: (MonadState SymbolTable m, MonadError AnalysisError m)
            => Maybe Ident
            -> Ident
            -> Type
            -> m ()
insertScope Nothing name ty = do
  (SymbolTable fns globs locs) <- get
  globs' <- M.alterF (\case
     Nothing -> pure $ Just ty
     Just _ -> throwError $ AlreadyDefined name) name globs
  put $ SymbolTable fns globs' locs
insertScope (Just fn) name ty = do
  (SymbolTable fns globs locs) <- get
  let loc = locs M.! fn
  loc' <- M.alterF (\case
     Nothing -> pure $ Just ty
     Just _ -> throwError $ AlreadyDefined name) name loc
  put $ SymbolTable fns globs (M.insert fn loc' locs)

symbolTable :: Program -> Either AnalysisError SymbolTable
symbolTable prog = do
  fns <- fnTable prog
  let initTab = SymbolTable fns M.empty (M.empty <$ fns)
  execStateT (mapM_ visitItem prog) initTab
  where
    visitItem (StmtItem stmt) = visitStmt Nothing stmt
    visitItem (FnDefItem (FnDef fn _ _ body)) = mapM_ (visitStmt $ Just fn) body
    visitStmt s (Decl name ty _) = insertScope s name ty
    visitStmt s (Assign e1 e2) = visitExpr s e1 >> visitExpr s e2
    visitStmt s (AssignOp _ e1 e2) = visitExpr s e1 >> visitExpr s e2
    visitStmt s (Input e) = visitExpr s e
    visitStmt s (Output e) = visitExpr s e
    visitStmt s (Return (Just e)) = visitExpr s e
    visitStmt s (ExprStmt e) = visitExpr s e
    visitExpr s (Var name) = do
      tab <- get
      case getSymbolInfo tab s name of
        Just _ -> pure ()
        Nothing -> throwError $ Undefined name
    visitExpr s (FnCall e es) = visitExpr s e >> mapM_ (visitExpr s) es
    visitExpr s (UnOpApp _ e) = visitExpr s e
    visitExpr s (BinOpApp _ e1 e2) = visitExpr s e1 >> visitExpr s e2
    visitExpr _ _ = pure ()

fnTable :: Program -> Either AnalysisError FnTable
fnTable = flip execStateT M.empty . mapM_ visitItem where
  visitItem (FnDefItem (FnDef name args ret _)) = do
    tab <- get
    tab' <- M.alterF (\case
      Nothing -> pure $ Just (args, ret)
      Just _ -> throwError $ AlreadyDefined name) name tab
    put tab'
  visitItem _ = pure ()

typecheck :: MonadError AnalysisError m => SymbolTable -> Program -> m ()
typecheck tab = mapM_ checkItem where
  checkItem (StmtItem stmt) = checkStmt tab Nothing stmt
  checkItem (FnDefItem fndef) = checkFnDef tab fndef
{-# INLINE typecheck #-}

checkStmt :: MonadError AnalysisError m
          => SymbolTable
          -> Maybe Ident
          -> Stmt
          -> m ()
checkStmt tab s (ExprStmt e) = checkExpr tab s e >> pure ()

checkFnDef :: MonadError AnalysisError m => SymbolTable -> FnDef -> m ()
checkFnDef = undefined

checkExpr :: MonadError AnalysisError m
          => SymbolTable
          -> Maybe Ident
          -> Expr
          -> m Type
checkExpr tab s (Var x) = case getSymbolInfo tab s x of
  Just (SymbolFn args ret) -> pure $ FnType (snd <$> args) ret
  Just (SymbolVar ty) -> pure ty
  Nothing -> throwError $ Undefined x
checkExpr _ _ (Lit _) = pure IntType
checkExpr tab s (FnCall f args) = do
  fnTy <- checkExpr tab s f
  argTys <- traverse (checkExpr tab s) args
  case fnTy of
    FnType prmTys ret -> do
      mapM_ checkArg (zip prmTys argTys)
      pure ret
    PtrType (FnType prmTys ret) -> do
      mapM_ checkArg (zip prmTys argTys)
      pure ret
    ty -> throwError $ ExpectedFnType ty
  where
    checkArg (declTy, argTy) = if declTy == argTy
      then pure ()
      else throwError $ TypeError declTy argTy
checkExpr tab s (UnOpApp AddressOf e) = do
  when (not $ lvalue e) (throwError $ NonLValue e)
  ty <- checkExpr tab s e
  when (not $ pointable ty) (throwError $ NonPointableType ty)
  pure $ PtrType ty
checkExpr tab s (UnOpApp DeRef e) = do
  ty <- checkExpr tab s e
  case ty of
    PtrType pTy -> pure pTy
    _ -> throwError $ ExpectedPtrType ty
checkExpr tab s (UnOpApp Negate e) = do
  ty <- checkExpr tab s e
  case ty of
    IntType -> pure IntType
    _ -> throwError $ TypeError IntType ty
checkExpr tab s (BinOpApp op e1 e2) = do
  ty1 <- checkExpr tab s e1
  ty2 <- checkExpr tab s e2
  promote op ty1 ty2

concrete :: Type -> Bool
concrete (FnType _ _) = False
concrete UnitType = False
concrete _ = True
{-# INLINE concrete #-}

pointable :: Type -> Bool
pointable UnitType = False
pointable _ = True
{-# INLINE pointable #-}

lvalue :: Expr -> Bool
lvalue (Var _) = True
lvalue (UnOpApp DeRef _) = True
lvalue _ = False
{-# INLINE lvalue #-}

promote :: MonadError AnalysisError m => BinaryOp -> Type -> Type -> m Type
promote _ IntType IntType = pure IntType
promote Add p@(PtrType _) IntType = pure p
promote Add IntType p@(PtrType _) = pure p
promote Add ty IntType = throwError $ TypeError IntType ty
promote Add _ ty = throwError $ TypeError IntType ty
promote Mul ty IntType = throwError $ TypeError IntType ty
promote Mul _ ty = throwError $ TypeError IntType ty
promote Eql _ _ = pure IntType -- TODO: this is shady
promote Less _ _ = pure IntType
promote LessEql _ _ = pure IntType
promote LogAnd _ _ = pure IntType
promote LogOr _ _ = pure IntType
{-# INLINE promote #-}
