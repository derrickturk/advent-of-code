{-# LANGUAGE OverloadedRecordDot, OverloadedStrings #-}

import Data.List (sortBy)
import qualified Data.Map.Strict as M

import FemtoParsec

data Operand
  = Old
  | Lit Int
  deriving Show

data Op
  = Add
  | Mul
  deriving Show

data Monkey
  = Monkey { items :: [Int]
           , operation :: (Op, Operand)
           , test :: Int
           , ifTrue :: Int
           , ifFalse :: Int
           , seen :: Int
           }
  deriving Show

type MonkeyWorld = M.Map Int Monkey

apply :: Monkey -> Int -> (Bool, Int)
apply m x = let x' = apply' m.operation x `div` 3
                apply' (Add, Old) = (* 2)
                apply' (Add, Lit n) = (+ n)
                apply' (Mul, Old) = (^ (2::Int))
                apply' (Mul, Lit n) = (* n)
             in (x' `mod` m.test == 0, x')

runMonkey :: Int -> MonkeyWorld -> MonkeyWorld
runMonkey ix world =
  M.adjust (\m -> m { items = [], seen = m.seen + incr }) ix world' where
    thisMonkey = world M.! ix
    (world', incr) = go world 0 thisMonkey.items
    go w n [] = (w, n)
    go w n (i:is) =
      let (test, i') = apply thisMonkey i
          send who val = M.adjust (\m -> m { items = m.items <> [val] }) who w
       in if test
            then go (send thisMonkey.ifTrue i') (n + 1) is
            else go (send thisMonkey.ifFalse i') (n + 1) is

runRound :: MonkeyWorld -> MonkeyWorld
runRound world = go (M.keys world) world where
  go [] w = w
  go (ix:rest) w = go rest (runMonkey ix w)

monkeyP :: Parser (Int, Monkey)
monkeyP = do
  ix <- lexeme "Monkey" *> unsignedIntNum <* lexeme ":"
  items <- lexeme "Starting items:" *> lexeme (sepBy ", " unsignedIntNum)
  op <- lexeme "Operation: new = old" *> lexeme (Add <$ "+" <|> Mul <$ "*")
  opnd <- lexeme (Old <$ "old" <|> Lit <$> unsignedIntNum)
  test <- lexeme "Test: divisible by" *> lexeme unsignedIntNum
  t <- lexeme "If true: throw to monkey" *> lexeme unsignedIntNum
  f <- lexeme "If false: throw to monkey" *> unsignedIntNum
  pure $ (ix, Monkey items (op, opnd) test t f 0)

main :: IO ()
main = do
  Just monkeys <- fmap M.fromList <$> parseStdin (some (lexeme monkeyP))
  let worlds = iterate runRound monkeys
      world20 = worlds !! 20
      monkeys20 = sortBy (\m1 m2 -> compare m2.seen m1.seen) $ M.elems world20
  (m1:m2:_) <- pure monkeys20
  print $ m1.seen * m2.seen
