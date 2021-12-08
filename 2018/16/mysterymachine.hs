{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (foldM)
import Data.Bits
import Data.List (foldl')
import Prelude hiding (read)

import FemtoParsec

data Opcode
  = Addr
  | Addi
  | Mulr
  | Muli
  | Banr
  | Bani
  | Borr
  | Bori
  | Setr
  | Seti
  | Gtir
  | Gtri
  | Gtrr
  | Eqir
  | Eqri
  | Eqrr
  deriving (Eq, Show)

data Instr = Instr Opcode Int Int Int
  deriving (Eq, Show)

data Error
  = InvalidRegister Int
  | InvalidRegisterCount
  deriving (Eq, Show)

data Cpu = Cpu { registers :: (Int, Int, Int, Int) }
  deriving (Eq, Show)

data Example = Example { before :: Cpu
                       , after :: Cpu
                       , code :: (Int, Int, Int, Int)
                       } deriving (Eq, Show)

allOpcodes :: [Opcode]
allOpcodes =
  [ Addr
  , Addi
  , Mulr
  , Muli
  , Banr
  , Bani
  , Borr
  , Bori
  , Setr
  , Seti
  , Gtir
  , Gtri
  , Gtrr
  , Eqir
  , Eqri
  , Eqrr
  ]

makeCpu :: [Int] -> Either Error Cpu
makeCpu [a, b, c, d] = Right $ Cpu (a, b, c, d)
makeCpu _ = Left InvalidRegisterCount

read :: Int -> Cpu -> Either Error Int
read 0 (Cpu (a, _, _, _)) = Right a
read 1 (Cpu (_, b, _, _)) = Right b
read 2 (Cpu (_, _, c, _)) = Right c
read 3 (Cpu (_, _, _, d)) = Right d
read r _ = Left $ InvalidRegister r

write :: Int -> Int -> Cpu -> Either Error Cpu
write 0 x (Cpu (_, b, c, d)) = Right $ Cpu (x, b, c, d)
write 1 x (Cpu (a, _, c, d)) = Right $ Cpu (a, x, c, d)
write 2 x (Cpu (a, b, _, d)) = Right $ Cpu (a, b, x, d)
write 3 x (Cpu (a, b, c, _)) = Right $ Cpu (a, b, c, x)
write r _ _ = Left $ InvalidRegister r

binopRR :: (Int -> Int -> Int) -> Int -> Int -> Int -> Cpu -> Either Error Cpu
binopRR op a b c cpu = do
  a' <- read a cpu
  b' <- read b cpu
  write c (op a' b') cpu

binopRI :: (Int -> Int -> Int) -> Int -> Int -> Int -> Cpu -> Either Error Cpu
binopRI op a b c cpu = do
  a' <- read a cpu
  write c (op a' b) cpu

testRR :: (Int -> Int -> Bool) -> Int -> Int -> Int -> Cpu -> Either Error Cpu
testRR test a b c cpu = do
  a' <- read a cpu
  b' <- read b cpu
  write c (if test a' b' then 1 else 0) cpu

testRI :: (Int -> Int -> Bool) -> Int -> Int -> Int -> Cpu -> Either Error Cpu
testRI test a b c cpu = do
  a' <- read a cpu
  write c (if test a' b then 1 else 0) cpu

testIR :: (Int -> Int -> Bool) -> Int -> Int -> Int -> Cpu -> Either Error Cpu
testIR test a b c cpu = do
  b' <- read b cpu
  write c (if test a b' then 1 else 0) cpu

step :: Cpu -> Instr -> Either Error Cpu
step cpu (Instr Addr a b c) = binopRR (+) a b c cpu
step cpu (Instr Addi a b c) = binopRI (+) a b c cpu
step cpu (Instr Mulr a b c) = binopRR (*) a b c cpu
step cpu (Instr Muli a b c) = binopRI (*) a b c cpu
step cpu (Instr Banr a b c) = binopRR (.&.) a b c cpu
step cpu (Instr Bani a b c) = binopRI (.&.) a b c cpu
step cpu (Instr Borr a b c) = binopRR (.|.) a b c cpu
step cpu (Instr Bori a b c) = binopRI (.|.) a b c cpu
step cpu (Instr Setr a _ c) = read a cpu >>= \v -> write c v cpu
step cpu (Instr Seti a _ c) = write c a cpu
step cpu (Instr Gtir a b c) = testIR (>) a b c cpu
step cpu (Instr Gtri a b c) = testRI (>) a b c cpu
step cpu (Instr Gtrr a b c) = testRR (>) a b c cpu
step cpu (Instr Eqir a b c) = testIR (==) a b c cpu
step cpu (Instr Eqri a b c) = testRI (==) a b c cpu
step cpu (Instr Eqrr a b c) = testRR (==) a b c cpu

run :: Cpu -> [Instr] -> Either Error Cpu
run cpu is = foldM step cpu is 

exampleP :: Parser Example
exampleP = do
  _ <- "Before: ["
  Right bs <- makeCpu <$> sepBy ", " intNum
  _ <- lexeme "]"
  [o, a, b, c] <- lexeme $ sepBy " " intNum
  _ <- "After:  ["
  Right as <- makeCpu <$> sepBy ", " intNum
  _ <- "]"
  pure $ Example bs as (o, a, b, c)

progP :: Parser [(Int, Int, Int, Int)]
progP = many $ do
  [o, a, b, c] <- lexeme $ sepBy " " intNum
  pure (o, a, b, c)

checkEx :: Example -> (Int, [Opcode])
checkEx (Example bs as (n, a, b, c)) = (n, filter f allOpcodes) where
  f op = step bs (Instr op a b c) == Right as

possibleOpcodes :: [Example] -> [(Int, [Opcode])]
possibleOpcodes = foldl' refine allPoss where
  allPoss :: [(Int, [Opcode])]
  allPoss = zip [0..15] $ repeat allOpcodes
  refine ps ex = let (i, ops) = checkEx ex
                  in alter i (whereOnly ops) ps
  whereOnly these those = filter (`elem` these) those
  alter _ _ [] = []
  alter x f ((y, v):rest)
    | y == x = (x, f v):rest
    | otherwise = (y, v):alter x f rest

propagate :: [(Int, [Opcode])] -> Maybe [(Int, Opcode)]
propagate opmap
  | all ((== 1) . length . snd) opmap =
      Just $ (\(n, os) -> (n, head os)) <$> opmap
  | otherwise = if opmap' == opmap
                  then Nothing
                  else propagate opmap'
  where
    opmap' = foldl' refine opmap (filter ((== 1) . length . snd) opmap)
    refine :: [(Int, [Opcode])] -> (Int, [Opcode]) -> [(Int, [Opcode])]
    refine om (n, [op]) = fmap (prop1 n op) om
    refine _ _ = error "internal goof"
    prop1 n op (m, os)
      | n == m = (m, os)
      | otherwise = (m, filter (/= op) os)

mapOpcodes :: [Example] -> Maybe [(Int, Opcode)]
mapOpcodes = propagate . possibleOpcodes

decode :: [(Int, Opcode)] -> (Int, Int, Int, Int) -> Maybe Instr
decode m (o, a, b, c) = Instr <$> lookup o m <*> pure a <*> pure b <*> pure c

main :: IO ()
main = do
  Just (ex, prog) <- parseStdin $ (,) <$> many (lexeme exampleP) <*> progP
  print $ length $ filter (\e -> length (checkEx e) >= 3) ex
  let Just opmap = mapOpcodes ex
      Just prog' = traverse (decode opmap) prog
      Right cpu = run (Cpu (0, 0, 0, 0)) prog'
  print cpu
