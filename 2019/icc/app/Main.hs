module Main where

import IntCodeC.Asm

main :: IO ()
main = print $
  (Instr (Add (Anon (Immediate 3)) (Anon (Immediate 3)) (Anon (Position 0))))
