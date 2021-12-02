{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Array as A
import Data.Char (isAsciiLower)
import Data.Tuple (swap)
import Prelude hiding (read)

import FemtoParsec
import Q

data Src
  = Val Int
  | ReadReg Char
  deriving (Eq, Show)

data Dst
  = WriteReg Char
  deriving (Eq, Show)

data Instr
  = Snd Src
  | Set Dst Src
  | Add Dst Src
  | Mul Dst Src
  | Mod Dst Src
  | Rcv Dst
  | Jgz Src Src
  deriving (Eq, Show)

data NoStep
  = Blocked
  | Halted
  deriving (Eq, Show)

data CPU
  = CPU { code :: A.Array Int Instr
        , ip :: Int
        , registers :: A.Array Char Int
        , out :: Q Int
        , sendCount :: Int
        } deriving (Show)

initCPU :: Int -> [Instr] -> CPU
initCPU cpuid instrs =
  let bounds = (0, length instrs - 1)
   in CPU { code = A.listArray bounds instrs
          , ip = 0
          , registers = (A.listArray ('a', 'z') $ repeat 0) A.// [('p', cpuid)]
          , out = Empty
          , sendCount = 0
          }

read :: CPU -> Src -> Int
read _ (Val x) = x
read cpu (ReadReg r) = registers cpu A.! r

modify :: CPU -> Dst -> (Int -> Int) -> CPU
modify cpu (WriteReg r) f = let rs = registers cpu 
                             in cpu { registers = rs A.// [(r, f (rs A.! r))] }

write :: CPU -> Dst -> Int -> CPU
write cpu dst x = modify cpu dst $ const x

next :: CPU -> Either NoStep Instr
next cpu
  | A.inRange (A.bounds $ code cpu) (ip cpu) = Right $ code cpu A.! ip cpu
  | otherwise = Left Halted

step :: CPU -> CPU -> Instr -> Either NoStep (CPU, CPU)
step cpu other (Snd src) = Right $
  ( cpu { ip = ip cpu + 1
        , out = out cpu <: read cpu src
        , sendCount = sendCount cpu + 1
        }
  , other
  )
step cpu other (Set dst src) = Right $
  ( (write cpu dst (read cpu src)) { ip = ip cpu + 1 }
  , other
  )
step cpu other (Add dst src) = Right $
  ( (modify cpu dst (+ (read cpu src))) { ip = ip cpu + 1 }
  , other
  )
step cpu other (Mul dst src) = Right $
  ( (modify cpu dst (* (read cpu src))) { ip = ip cpu + 1 }
  , other
  )
step cpu other (Mod dst src) = Right $
  ( (modify cpu dst (`mod` (read cpu src))) { ip = ip cpu + 1 }
  , other
  )
step cpu other (Rcv dst)
  | (x :> xs) <- (out other) = Right $
      ( (write cpu dst x) { ip = ip cpu + 1 }
      , other { out = xs }
      )
  | otherwise = Left Blocked
step cpu other (Jgz cnd off) = Right $
  let off' = if read cpu cnd > 0 then read cpu off else 1
   in ( cpu { ip = ip cpu + off' }
      , other
      )

run :: (CPU, CPU) -> (CPU, CPU)
run (cpu1, cpu2) = case next cpu1 >>= step cpu1 cpu2 of
  Right (cpu1', cpu2') -> swap $ run (cpu2', cpu1')
  Left _ -> case next cpu2 >>= step cpu2 cpu1 of
    Left _ -> (cpu1, cpu2)
    Right (cpu2', cpu1') -> run (cpu1', cpu2')

trace :: (CPU, CPU) -> [(CPU, CPU)]
trace (cpu1, cpu2) = case next cpu1 >>= step cpu1 cpu2 of
  Right (cpu1', cpu2') -> (cpu1', cpu2'):(swap <$> trace (cpu2', cpu1'))
  Left _ -> case next cpu2 >>= step cpu2 cpu1 of
    Left _ -> []
    Right (cpu2', cpu1') -> (cpu1', cpu2'):(trace (cpu1', cpu2'))

reg :: Parser Char
reg = charP isAsciiLower

srcP :: Parser Src
srcP =  Val <$> intNum
    <|> ReadReg <$> reg

dstP :: Parser Dst
dstP = WriteReg <$> reg

instr :: Parser Instr
instr =  Snd <$> (lexeme "snd" *> srcP)
     <|> Set <$> (lexeme "set" *> lexeme dstP) <*> srcP
     <|> Add <$> (lexeme "add" *> lexeme dstP) <*> srcP
     <|> Mul <$> (lexeme "mul" *> lexeme dstP) <*> srcP
     <|> Mod <$> (lexeme "mod" *> lexeme dstP) <*> srcP
     <|> Rcv <$> (lexeme "rcv" *> dstP)
     <|> Jgz <$> (lexeme "jgz" *> lexeme srcP) <*> srcP

main :: IO ()
main = do
  Just instrs <- parseStdin $ many $ lexeme instr
  let cpu1 = initCPU 0 instrs
      cpu2 = initCPU 1 instrs
  print $ sendCount $ snd $ run (cpu1, cpu2)
