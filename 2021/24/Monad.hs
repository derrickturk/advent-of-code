{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Monad (
    Var(..)
  , Src(..)
  , CPU(..)
  , Instr(..)
  , State(..)
  , Compute(..)
  , read
  , write
  , binOp
  , run
  , program
) where

import Prelude hiding (read)
import Control.Monad (foldM)

import FemtoParsec

data Var = W | X | Y | Z
  deriving (Eq, Show)

data Src
  = Mem Var
  | Lit Int
  deriving (Eq, Show)

data CPU a
  = CPU { w :: a
        , x :: a
        , y :: a
        , z :: a
        } deriving (Eq, Show)

data Instr
  = Input Var
  | Add Var Src
  | Mul Var Src
  | Div Var Src
  | Mod Var Src
  | Eql Var Src
  deriving (Eq, Show)

data State a input = State (CPU a) input
  deriving (Eq, Show)

class Compute a input | a -> input where
  boot :: CPU a
  liftConst :: Int -> a
  step :: Instr -> State a input -> Maybe (State a input)

read :: Compute a input => Src -> CPU a -> a
read (Mem W) = w
read (Mem X) = x
read (Mem Y) = y
read (Mem Z) = z
read (Lit n) = const (liftConst n)

write :: a -> Var -> CPU a -> CPU a
write n W c = c { w = n }
write n X c = c { x = n }
write n Y c = c { y = n }
write n Z c = c { z = n }

binOp :: Compute a input => (a -> a -> a) -> Var -> Src -> CPU a -> CPU a
binOp f v o c =
  let val = f (read (Mem v) c) (read o c)
   in write val v c

run :: Compute a input => [Instr] -> State a input -> Maybe (State a input)
run is s = foldM (flip step) s is

instance Compute Int [Int] where
  boot = CPU 0 0 0 0

  liftConst n = n

  step (Input v) (State c (i:is)) = Just $ State (write i v c) is
  step (Input _) (State _ []) = Nothing
  step (Add v o) (State c is) = Just $ State (binOp (+) v o c) is
  step (Mul v o) (State c is) = Just $ State (binOp (*) v o c) is
  step (Div v o) (State c is) = Just $ State (binOp div v o c) is
  step (Mod v o) (State c is) = Just $ State (binOp mod v o c) is
  step (Eql v o) (State c is) = Just $
    State (binOp (\a b -> if a == b then 1 else 0) v o c) is

var :: Parser Var
var =  W <$ char 'w'
   <|> X <$ char 'x'
   <|> Y <$ char 'y'
   <|> Z <$ char 'z'

src :: Parser Src
src =  Mem <$> var
   <|> Lit <$> intNum

instr :: Parser Instr
instr =  Input <$> (lexeme "inp" *> var)
     <|> Add <$> (lexeme "add" *> lexeme var) <*> src
     <|> Mul <$> (lexeme "mul" *> lexeme var) <*> src
     <|> Div <$> (lexeme "div" *> lexeme var) <*> src
     <|> Mod <$> (lexeme "mod" *> lexeme var) <*> src
     <|> Eql <$> (lexeme "eql" *> lexeme var) <*> src

program :: Parser [Instr]
program = some $ lexeme instr
