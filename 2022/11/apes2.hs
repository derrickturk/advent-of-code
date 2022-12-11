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

apply :: Int -> Monkey -> Int -> (Bool, Int)
apply base m x = let x' = apply' m.operation x `mod` base
                     apply' (Add, Old) = (* 2)
                     apply' (Add, Lit n) = (+ n)
                     apply' (Mul, Old) = (^ (2::Int))
                     apply' (Mul, Lit n) = (* n)
                  in (x' `mod` m.test == 0, x')

runMonkey :: Int -> Int -> MonkeyWorld -> MonkeyWorld
runMonkey base ix world =
  M.adjust (\m -> m { items = [], seen = m.seen + incr }) ix world' where
    thisMonkey = world M.! ix
    (world', incr) = go world 0 thisMonkey.items
    go w n [] = (w, n)
    go w n (i:is) =
      let (test, i') = apply base thisMonkey i
          send who val = M.adjust (\m -> m { items = m.items <> [val] }) who w
       in if test
            then go (send thisMonkey.ifTrue i') (n + 1) is
            else go (send thisMonkey.ifFalse i') (n + 1) is

runRound :: Int -> MonkeyWorld -> MonkeyWorld
runRound base world = go (M.keys world) world where
  go [] w = w
  go (ix:rest) w = go rest (runMonkey base ix w)

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
  {- (I'm pretty confident that) for any set of "test" values t_i,
   -   ∀ x . x mod (∏i ti) == x mod t_i
   - (in our particular case all t_i are different and prime)
   -}
  let base = product $ (\m -> m.test) <$> M.elems monkeys
      worlds = iterate (runRound base) monkeys
      world10k = worlds !! 10000
      monkeys10k = sortBy (\m1 m2 -> compare m2.seen m1.seen) $ M.elems world10k
  (m1:m2:_) <- pure monkeys10k
  print $ m1.seen * m2.seen
