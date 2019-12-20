{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}

module Main where

import System.Environment
import System.IO
import qualified Data.Text.IO as TIO
-- import System.FilePath

import qualified Text.Megaparsec as MP

-- import Prufrock.Asm
import Prufrock.Analyze
-- import Prufrock.Grammar
import Prufrock.Parser

{-
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
  MP.parseTest item "f(23);"
  MP.parseTest program " x: int; y: *int = &x; *y = 3; output x; "
-}

compileFile :: FilePath -> IO ()
compileFile path = do
  -- let outPath = dropExtension path <.> "ica"
  src <- TIO.readFile path
  case MP.parse program path src of
    Left e -> hPutStrLn stderr $ MP.errorBundlePretty e
    Right prog -> do
      let checked = do
            tab <- symbolTable prog
            typecheck tab prog
      case checked of
        Left e -> hPutStrLn stderr $ show e
        Right _ -> pure ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> hPutStrLn stderr "Usage: prc files..."
    _ -> mapM_ compileFile args
