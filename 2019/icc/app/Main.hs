{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}

module Main where

import IntCodeC.Asm
import System.IO (stdout)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  hEmit stdout
    (Instr (Add (Anon (Imm (Num 3))) (Anon (Imm (Num 3))) (Anon (Abs (Num 0)))))
  TIO.hPutStrLn stdout ""
