{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}

module Main where

import System.IO (stdout)
import qualified Data.Text.IO as TIO

import qualified Text.Megaparsec as MP

import Prufrock.Asm
import Prufrock.Parser

emitTest :: IO ()
emitTest = do
  hEmit stdout
    (Instr (Add (Anon (Imm (Num 3))) (Anon (Imm (Num 3))) (Anon (Abs (Num 0)))))
  TIO.hPutStrLn stdout ""

parseTest :: IO ()
parseTest = do
  MP.parseTest ty "int"
  MP.parseTest ty "***int"
  MP.parseTest ty "fn( int, *int ) -> ***int"
  MP.parseTest ty "fn( fn(int) -> int, *int )"
  MP.parseTest ident "xyz"
  MP.parseTest ident "_Azrael"
  MP.parseTest expr "23"
  MP.parseTest expr "-23"
  MP.parseTest expr "**p <= ----23"
  MP.parseTest stmt "x: int = 23 < 5;"
  MP.parseTest stmt "p: *int;"
  MP.parseTest stmt "return p;"
  MP.parseTest stmt "output f(521);"
  MP.parseTest stmt "output f(521, *p);"
  MP.parseTest stmt "output f(-521, *p)(**x < 7)(&y <= 3) && 5;"
  MP.parseTest item "fn f(x: int) -> *int { return &x; }"
  MP.parseTest item "x: int = 3;"
  MP.parseTest program " x: int; y: *int = &x; *y = 3; output x; "

main :: IO ()
main = do
  emitTest
  parseTest
