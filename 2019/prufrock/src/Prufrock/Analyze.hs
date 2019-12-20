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

import Data.List (find)
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
  | NoReturn Ident Type
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
    visitItem (FnDefItem (FnDef fn args _ body)) = do
      mapM_ (\(name, ty) -> insertScope (Just fn) name ty) args
      mapM_ (visitStmt $ Just fn) body
    visitStmt s (Decl name ty Nothing) = insertScope s name ty
    visitStmt s (Decl name ty (Just e)) = visitExpr s e >> insertScope s name ty
    visitStmt s (Assign e1 e2) = visitExpr s e1 >> visitExpr s e2
    visitStmt s (AssignOp _ e1 e2) = visitExpr s e1 >> visitExpr s e2
    visitStmt s (Input e) = visitExpr s e
    visitStmt s (Output e) = visitExpr s e
    visitStmt s (Return (Just e)) = visitExpr s e
    visitStmt _ (Return Nothing) = pure ()
    visitStmt _ Exit = pure ()
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
checkStmt _ _ (Decl _ ty Nothing) =
  when (not $ concrete ty) (throwError $ NonConcreteType ty)
checkStmt tab s (Decl _ ty (Just e)) = do
  when (not $ concrete ty) (throwError $ NonConcreteType ty) 
  eTy <- checkExpr tab s e
  when (not $ coerce eTy ty) (throwError $ TypeError ty eTy)
checkStmt tab s (Assign e1 e2) = do
  when (not $ lvalue e1) (throwError $ NonLValue e1) 
  e1Ty <- concreteExpr tab s e1
  e2Ty <- checkExpr tab s e2
  when (not $ coerce e2Ty e1Ty) (throwError $ TypeError e1Ty e2Ty)
checkStmt tab s (AssignOp op e1 e2) = do
  e1Ty <- concreteExpr tab s e1
  e2Ty <- concreteExpr tab s e2
  prTy <- promote op e1Ty e2Ty
  when (not $ prTy == e1Ty) (throwError $ TypeError e1Ty prTy)
checkStmt tab s (Input e) = do
  when (not $ lvalue e) (throwError $ NonLValue e)
  _ <- concreteExpr tab s e
  pure ()
checkStmt tab s (Output e) = do
  _ <- concreteExpr tab s e
  pure ()
checkStmt _ _ (Return Nothing) = pure ()
checkStmt tab s (Return (Just e)) = do 
  _ <- concreteExpr tab s e
  pure ()
checkStmt _ _ Exit = pure ()
checkStmt tab s (ExprStmt e) = checkExpr tab s e >> pure ()

checkFnDef :: MonadError AnalysisError m => SymbolTable -> FnDef -> m ()
checkFnDef tab (FnDef fn args ret body) = do
  mapM_ checkArg args
  mapM_ (checkStmt tab (Just fn)) body
  case ret of
    UnitType -> checkUnitReturns
    ty -> if (not $ concrete ty)
      then throwError $ NonConcreteType ty
      else checkReturns ty
  where
    checkArg (_, declTy) = do
      when (not $ concrete declTy) (throwError $ NonConcreteType declTy)
    allReturns [] = pure []
    allReturns ((Return Nothing):rest) = (:) <$> pure UnitType
                                             <*> allReturns rest
    allReturns ((Return (Just e)):rest) = (:) <$> checkExpr tab (Just fn) e
                                              <*> allReturns rest
    allReturns (_:rest) = allReturns rest
    checkUnitReturns = do
      rets <- allReturns body
      case find (/= UnitType) rets of
        Nothing -> pure ()
        Just ty -> throwError $ TypeError UnitType ty
    checkReturns ty = do
      rets <- allReturns body
      when (null rets) (throwError $ NoReturn fn ty)
      case find (/= ty) rets of
        Nothing -> pure ()
        Just otherTy -> throwError $ TypeError ty otherTy

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
  argTys <- traverse (concreteExpr tab s) args
  case fnTy of
    FnType prmTys ret -> do
      mapM_ checkArg (zip prmTys argTys)
      pure ret
    PtrType (FnType prmTys ret) -> do
      mapM_ checkArg (zip prmTys argTys)
      pure ret
    ty -> throwError $ ExpectedFnType ty
  where
    checkArg (declTy, argTy) = do
      when (not $ concrete declTy) (throwError $ NonConcreteType declTy)
      when (declTy /= argTy) (throwError $ TypeError declTy argTy)
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
  ty1 <- concreteExpr tab s e1
  ty2 <- concreteExpr tab s e2
  promote op ty1 ty2

concreteExpr :: MonadError AnalysisError m
             => SymbolTable
             -> Maybe Ident
             -> Expr
             -> m Type
concreteExpr tab s e = do
  eTy <- checkExpr tab s e
  when (not $ concrete eTy) (throwError $ NonConcreteType eTy)
  pure eTy
{-# INLINE concreteExpr #-}

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

-- coerce from to
coerce :: Type -> Type -> Bool
coerce IntType (PtrType _) = True
coerce (FnType fnArgs fnRet) (PtrType (FnType pArgs pRet))
  | fnArgs == pArgs && fnRet == pRet = True
  | otherwise = False
coerce ty1 ty2 = ty1 == ty2
{-# INLINE coerce #-}
