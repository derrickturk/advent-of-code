{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Array as A
import Data.Char (isAsciiLower)
import Prelude hiding (read)

import FemtoParsec

data Src
  = Val Int
  | ReadReg Char
  deriving (Eq, Show)

data Dst
  = WriteReg Char
  deriving (Eq, Show)

data Instr
  = Set Dst Src
  | Sub Dst Src
  | Mul Dst Src
  | Jnz Src Src
  deriving (Eq, Show)

data CPU
  = CPU { code :: A.Array Int Instr
        , ip :: Int
        , registers :: A.Array Char Int
        } deriving (Eq, Show)

initCPU :: [Instr] -> CPU
initCPU instrs =
  let bounds = (0, length instrs - 1)
   in CPU { code = A.listArray bounds instrs
          , ip = 0
          , registers = A.listArray ('a', 'h') $ repeat 0
          }

read :: CPU -> Src -> Int
read _ (Val x) = x
read cpu (ReadReg r) = registers cpu A.! r

modify :: CPU -> Dst -> (Int -> Int) -> CPU
modify cpu (WriteReg r) f = let rs = registers cpu 
                             in cpu { registers = rs A.// [(r, f (rs A.! r))] }

write :: CPU -> Dst -> Int -> CPU
write cpu dst x = modify cpu dst $ const x

asSrc :: Dst -> Src
asSrc (WriteReg r) = ReadReg r

next :: CPU -> Maybe Instr
next cpu
  | A.inRange (A.bounds $ code cpu) (ip cpu) = Just $ code cpu A.! ip cpu
  | otherwise = Nothing

step :: CPU -> Instr -> CPU
step cpu (Set dst src) =
  (write cpu dst (read cpu src)) { ip = ip cpu + 1 }
step cpu (Sub dst src) =
  (modify cpu dst (subtract (read cpu src))) { ip = ip cpu + 1 }
step cpu (Mul dst src) =
  (modify cpu dst (* (read cpu src))) { ip = ip cpu + 1 }
step cpu (Jnz cnd off) =
  let off' = if read cpu cnd /= 0 then read cpu off else 1
   in cpu { ip = ip cpu + off' }

run :: CPU -> CPU
run cpu = case step cpu <$> next cpu of
  Just cpu' -> run cpu'
  Nothing -> cpu

trace :: CPU -> [(CPU, Instr)]
trace cpu = case next cpu of
  Just i -> let cpu' = step cpu i
             in (cpu', i):trace cpu'
  Nothing -> []

reg :: Parser Char
reg = charP isAsciiLower

srcP :: Parser Src
srcP =  Val <$> intNum
    <|> ReadReg <$> reg

dstP :: Parser Dst
dstP = WriteReg <$> reg

instr :: Parser Instr
instr =  Set <$> (lexeme "set" *> lexeme dstP) <*> srcP
     <|> Sub <$> (lexeme "sub" *> lexeme dstP) <*> srcP
     <|> Mul <$> (lexeme "mul" *> lexeme dstP) <*> srcP
     <|> Jnz <$> (lexeme "jnz" *> lexeme srcP) <*> srcP

main :: IO ()
main = do
  Just instrs <- parseStdin $ many $ lexeme instr
  let cpu = initCPU instrs
  print $ length $ filter (isMul . snd) $ trace cpu
  where
    isMul (Mul _ _) = True
    isMul _ = False
