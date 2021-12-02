{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Array as A
import Data.Char (isAsciiLower)
import Data.List (find)
import Data.Maybe (isJust)
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
  = Snd Src
  | Set Dst Src
  | Add Dst Src
  | Mul Dst Src
  | Mod Dst Src
  | Rcv Src
  | Jgz Src Src
  deriving (Eq, Show)

data CPU
  = CPU { code :: A.Array Int Instr
        , ip :: Int
        , registers :: A.Array Char Int
        , lastSound :: Int
        } deriving (Eq, Show)

initCPU :: [Instr] -> CPU
initCPU instrs =
  let bounds = (0, length instrs - 1)
   in CPU { code = A.listArray bounds instrs
          , ip = 0
          , registers = A.listArray ('a', 'z') $ repeat 0
          , lastSound = 0
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

step :: CPU -> Instr -> (CPU, Maybe Int)
step cpu (Snd src) =
  ( cpu { ip = ip cpu + 1, lastSound = read cpu src }
  , Just $ read cpu src
  )
step cpu (Set dst src) =
  ( (write cpu dst (read cpu src)) { ip = ip cpu + 1 }
  , Nothing
  )
step cpu (Add dst src) =
  ( (modify cpu dst (+ (read cpu src))) { ip = ip cpu + 1 }
  , Nothing
  )
step cpu (Mul dst src) =
  ( (modify cpu dst (* (read cpu src))) { ip = ip cpu + 1 }
  , Nothing
  )
step cpu (Mod dst src) =
  ( (modify cpu dst (`mod` (read cpu src))) { ip = ip cpu + 1 }
  , Nothing
  )
step cpu (Rcv src) =
  ( cpu { ip = ip cpu + 1 }
  , if (read cpu src) /= 0
      then Just $ lastSound cpu
      else Nothing
  ) -- who knows?
step cpu (Jgz cnd off) =
  let off' = if read cpu cnd > 0 then read cpu off else 1
   in ( cpu { ip = ip cpu + off' }
      , Nothing
      )

run :: CPU -> CPU
run cpu = case step cpu <$> next cpu of
  Just (cpu', _) -> run cpu'
  Nothing -> cpu

trace :: CPU -> [(CPU, Instr, Maybe Int)]
trace cpu = case next cpu of
  Just i -> let (cpu', o) = step cpu i
             in (cpu', i, o):trace cpu'
  Nothing -> []

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
     <|> Rcv <$> (lexeme "rcv" *> srcP)
     <|> Jgz <$> (lexeme "jgz" *> lexeme srcP) <*> srcP

main :: IO ()
main = do
  Just instrs <- parseStdin $ many $ lexeme instr
  let cpu = initCPU instrs
      isRcv (Rcv _) = True
      isRcv _ = False
  let Just (_, _, Just frq) = find (\(_, i, o) -> isRcv i && isJust o) $ trace cpu
  print frq
