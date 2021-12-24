{-# LANGUAGE OverloadedStrings #-}

module Monad (
    Var(..)
  , Src(..)
  , CPU(..)
  , Instr(..)
  , State(..)
  , boot
  , read
  , write
  , step
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

data CPU
  = CPU { w :: Int
        , x :: Int
        , y :: Int
        , z :: Int
        } deriving (Eq, Show)

data Instr
  = Input Var
  | Add Var Src
  | Mul Var Src
  | Div Var Src
  | Mod Var Src
  | Eql Var Src
  deriving (Eq, Show)

data State = State CPU [Int]
  deriving (Eq, Show)

boot :: CPU
boot = CPU 0 0 0 0

read :: Src -> CPU -> Int
read (Mem W) = w
read (Mem X) = x
read (Mem Y) = y
read (Mem Z) = z
read (Lit n) = const n

write :: Int -> Var -> CPU -> CPU
write n W c = c { w = n }
write n X c = c { x = n }
write n Y c = c { y = n }
write n Z c = c { z = n }

binOp :: (Int -> Int -> Int) -> Var -> Src -> State -> State
binOp f v o (State c is) =
  let val = f (read (Mem v) c) (read o c)
   in State (write val v c) is

step :: Instr -> State -> Maybe State 
step (Input v) (State c (i:is)) = Just $ State (write i v c) is
step (Input _) (State _ []) = Nothing
step (Add v o) s = Just $ binOp (+) v o s
step (Mul v o) s = Just $ binOp (*) v o s
step (Div v o) s = Just $ binOp div v o s
step (Mod v o) s = Just $ binOp mod v o s
step (Eql v o) s = Just $ binOp (\a b -> if a == b then 1 else 0) v o s

run :: [Instr] -> State -> Maybe State
run is s = foldM (flip step) s is

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
