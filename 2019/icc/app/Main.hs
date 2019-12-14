module Main where

import IntCodeC.Asm

main :: IO ()
main = print $
  (Instr (Add (Anon (Imm (Num 3))) (Anon (Imm (Num 3))) (Anon (Abs (Num 0)))))
