{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Array as A
import Prelude hiding (read)

import FemtoParsec

data Reg = A | B | C | D
  deriving (Eq, Show)

data Src
  = Val Integer
  | ReadReg Reg
  deriving (Eq, Show)

data Dst
  = WriteReg Reg
  deriving (Eq, Show)

data Instr
  = Cpy Src Dst
  | Inc Dst
  | Dec Dst
  | Jnz Src Src
  deriving (Eq, Show)

data CPU
  = CPU { code :: A.Array Int Instr
        , ip :: Int
        , regA :: Integer
        , regB :: Integer
        , regC :: Integer
        , regD :: Integer
        } deriving (Eq, Show)

initCPU :: [Instr] -> CPU
initCPU instrs =
  let bounds = (0, length instrs - 1)
   in CPU { code = A.listArray bounds instrs
          , ip = 0
          , regA = 0
          , regB = 0
          , regC = 0
          , regD = 0
          }

read :: CPU -> Src -> Integer
read _ (Val x) = x
read cpu (ReadReg A) = regA cpu
read cpu (ReadReg B) = regB cpu
read cpu (ReadReg C) = regC cpu
read cpu (ReadReg D) = regD cpu

modify :: CPU -> Dst -> (Integer -> Integer) -> CPU
modify cpu (WriteReg A) f = cpu { regA = f (regA cpu) }
modify cpu (WriteReg B) f = cpu { regB = f (regB cpu) }
modify cpu (WriteReg C) f = cpu { regC = f (regC cpu) }
modify cpu (WriteReg D) f = cpu { regD = f (regD cpu) }

write :: CPU -> Dst -> Integer -> CPU
write cpu dst x = modify cpu dst $ const x

step :: CPU -> Maybe CPU
step cpu =
  let memBounds = A.bounds $ code cpu
   in if A.inRange memBounds (ip cpu)
     then Just $ case (code cpu) A.! (ip cpu) of
       Cpy src dst -> (write cpu dst (read cpu src)) { ip = ip cpu + 1 }
       Inc dst -> (modify cpu dst (+ 1)) { ip = ip cpu + 1 }
       Dec dst -> (modify cpu dst (subtract 1)) { ip = ip cpu + 1 }
       Jnz src n -> if read cpu src /= 0
         then cpu { ip = ip cpu + fromInteger (read cpu n) }
         else cpu { ip = ip cpu + 1 }
     else Nothing

run :: CPU -> CPU
run cpu = case step cpu of
  Just cpu' -> run cpu'
  Nothing -> cpu

trace :: CPU -> [CPU]
trace cpu = cpu:case step cpu of
  Just cpu' -> trace cpu'
  Nothing -> []

reg :: Parser Reg
reg =  A <$ char 'a'
   <|> B <$ char 'b'
   <|> C <$ char 'c'
   <|> D <$ char 'd'

srcP :: Parser Src
srcP =  Val <$> integer
    <|> ReadReg <$> reg

dstP :: Parser Dst
dstP = WriteReg <$> reg

instr :: Parser Instr
instr =  Cpy <$> (lexeme "cpy" *> lexeme srcP) <*> dstP
     <|> Inc <$> (lexeme "inc" *> dstP)
     <|> Dec <$> (lexeme "dec" *> dstP)
     <|> Jnz <$> (lexeme "jnz" *> lexeme srcP) <*> srcP

main :: IO ()
main = do
  Just instrs <- parseStdin $ many $ lexeme instr
  let cpu = initCPU instrs
  print $ regA $ run cpu
  print $ regA $ run cpu { regC = 1 }
