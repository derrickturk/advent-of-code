{-# LANGUAGE FlexibleContexts #-}

module Reggy (
    Reg(..)
  , Stmt(..)
  , RegOp(..)
  , Cond(..)
  , CmpOp(..)
  , Reggy
  , RegState
  , evalStmt
  , evalCond
  , evalCmpOp
  , runReggy
  , runReggyMaximum
) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Monad (when, foldM)
import Control.Monad.State.Strict

newtype Reg = Reg { getReg :: T.Text }
  deriving (Show, Eq, Ord)

data Stmt = Stmt Reg RegOp Int Cond
  deriving Show

data RegOp
  = Inc
  | Dec
  deriving Show

data Cond = Cond Reg CmpOp Int
  deriving Show

data CmpOp
  = Lt
  | Gt
  | LtEq
  | GtEq
  | Eq
  | NotEq
  deriving Show

type Reggy = [Stmt]

type RegState = M.Map Reg Int

readReg :: MonadState RegState m => Reg -> m Int
readReg r = do
  mem <- get
  let (val, mem') = M.insertLookupWithKey (\_ _ o -> o) r 0 mem
  put mem'
  pure $ fromMaybe 0 val

modifyReg :: MonadState RegState m => Reg -> (Int -> Int) -> m ()
modifyReg r f = readReg r >>= modify . M.insert r . f

maxReg :: MonadState RegState m => m Int
maxReg = do
  mem <- get
  if M.null mem
    then pure 0
    else pure $ maximum $ M.elems mem

evalStmt :: MonadState RegState m => Stmt -> m ()
evalStmt (Stmt r Inc n c) = evalCond c >>= flip when (modifyReg r (+ n))
evalStmt (Stmt r Dec n c) =
  evalCond c >>= flip when (modifyReg r (subtract n))

evalCond :: MonadState RegState m => Cond -> m Bool
evalCond (Cond r op n) = evalCmpOp op <$> readReg r <*> pure n

evalCmpOp :: CmpOp -> Int -> Int -> Bool
evalCmpOp Lt x y = x < y
evalCmpOp Gt x y = x > y
evalCmpOp LtEq x y = x <= y
evalCmpOp GtEq x y = x >= y
evalCmpOp Eq x y = x == y
evalCmpOp NotEq x y = x /= y

runReggy :: Reggy -> RegState
runReggy = (`execState` M.empty) . mapM_ evalStmt

runReggyMaximum :: Reggy -> (Int, RegState)
runReggyMaximum = (`runState` M.empty) . foldM step 0 where
  step m stmt = max m <$> (evalStmt stmt >> maxReg)
