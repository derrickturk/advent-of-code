{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Array as A
import Prelude hiding (read)
import Control.Monad (forM_)

import FemtoParsec

data Reg = A | B | C | D
  deriving (Eq, Show)

data Src
  = Val Integer
  | ReadReg Reg
  deriving (Eq, Show)

data Dst
  = WriteReg Reg
  | InvalidDstVal Integer
  deriving (Eq, Show)

data Instr
  = Cpy Src Dst
  | Inc Dst
  | Dec Dst
  | Jnz Src Src
  | Tgl Src
  | Mul Src Src Dst -- the forbidden instr!
  | Out Src
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
modify cpu (InvalidDstVal _) _ = cpu

write :: CPU -> Dst -> Integer -> CPU
write cpu dst x = modify cpu dst $ const x

toSrc :: Dst -> Src
toSrc (WriteReg r) = ReadReg r
toSrc (InvalidDstVal n) = Val n

toDst :: Src -> Dst
toDst (Val n) = InvalidDstVal n
toDst (ReadReg r) = WriteReg r

toggle :: Instr -> Instr
toggle (Cpy src dst) = Jnz src (toSrc dst)
toggle (Inc dst) = Dec dst
toggle (Dec dst) = Inc dst
toggle (Jnz src n) = Cpy src (toDst n)
toggle (Tgl n) = Inc (toDst n)
toggle (Mul _ _ _) = error "FORBIDDEN INSTRUCTION may not be TOGGLED!"
toggle (Out src) = Inc (toDst src)

step :: CPU -> Maybe (CPU, Maybe Integer)
step cpu =
  let memBounds = A.bounds $ code cpu
   in if A.inRange memBounds (ip cpu)
     then Just $ case (code cpu) A.! (ip cpu) of
       Cpy src dst ->
         ((write cpu dst (read cpu src)) { ip = ip cpu + 1 }, Nothing)
       Inc dst ->
         ((modify cpu dst (+ 1)) { ip = ip cpu + 1 }, Nothing)
       Dec dst ->
         ((modify cpu dst (subtract 1)) { ip = ip cpu + 1 }, Nothing)
       Jnz src n -> if read cpu src /= 0
         then (cpu { ip = ip cpu + fromInteger (read cpu n) }, Nothing)
         else (cpu { ip = ip cpu + 1 }, Nothing)
       Tgl n ->
         let i = ip cpu + fromInteger (read cpu n)
          in if A.inRange (A.bounds $ code cpu) i
               then (cpu { ip = ip cpu + 1
                         , code = code cpu A.// [(i, toggle (code cpu A.! i))]
                         }
                    , Nothing
                    )
               else (cpu { ip = ip cpu + 1 }, Nothing)
       Mul s1 s2 dst ->
         ((write cpu dst $ (read cpu s1) * (read cpu s2)) { ip = ip cpu + 1 }
         , Nothing
         )
       Out src -> (cpu { ip = ip cpu + 1 }, Just $ read cpu src)
     else Nothing

run :: CPU -> (CPU, [Integer])
run cpu =
  let (cpu', out) = go cpu []
      go c os = case step c of
        Just (c', Nothing) -> go c' os
        Just (c', Just o) -> go c' (o:os)
        Nothing -> (c, os)
   in (cpu', reverse out)

trace :: CPU -> [CPU]
trace cpu = cpu:case step cpu of
  Just (cpu', _) -> trace cpu'
  Nothing -> []

outputs :: CPU -> [Integer]
outputs cpu = case step cpu of
  Just (cpu', Just o) -> o:outputs cpu'
  Just (cpu', Nothing) -> outputs cpu'
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
     <|> Tgl <$> (lexeme "tgl" *> srcP)
     <|> Out <$> (lexeme "out" *> srcP)
     <|> Mul <$> (lexeme "mul" *> lexeme srcP)
             <*> (lexeme srcP)
             <*> dstP

main :: IO ()
main = do
  Just instrs <- parseStdin $ many $ lexeme instr
  let cpu = initCPU instrs
  forM_ [0..15] $ \i -> do
      putStr $ show i <> ": "
      print $ take 15 $ outputs $ cpu { regA = i }
