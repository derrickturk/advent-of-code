{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isAlpha)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import FemtoParsec

-- just the ones, ma'am
type Tape = S.Set Integer

data Dir = L | R
  deriving Show

type Rule = (Bool, Dir, Char)

type RuleSet = M.Map (Char, Bool) Rule

data Program = Program { initialState :: Char
                       , checksumAfter :: Int
                       , rules :: RuleSet
                       } deriving Show

type State = (Char, Tape, Integer)

move :: Dir -> Integer -> Integer
move L i = i - 1
move R i = i + 1

applyRules :: RuleSet -> State -> State
applyRules rs (s, t, i) = case M.lookup (s, S.member i t) rs of
  Just (True, dir, s') -> (s', S.insert i t, move dir i)
  Just (False, dir, s') -> (s', S.delete i t, move dir i)
  Nothing -> (s, t, i) -- halted I guess

run :: Program -> Int
run (Program initial n rs) = go 0 (initial, S.empty, 0) where
  go i st@(_, tape, _)
    | i == n = S.size tape
    | otherwise = go (i + 1) (applyRules rs st)

program :: Parser Program
program = do
  _ <- lexeme "Begin in state"
  s <- charP isAlpha
  _ <- lexeme "."
  _ <- lexeme "Perform a diagnostic checksum after"
  n <- lexeme unsignedIntNum
  _ <- lexeme "steps."
  rs <- concat <$> some rule
  pure $ Program s n $ M.fromList rs

rule :: Parser [((Char, Bool), Rule)]
rule = do
  _ <- "In state "
  s <- charP isAlpha
  _ <- lexeme ":"
  fmap (\(c, b, d, s') -> ((s, c), (b, d, s'))) <$> some (lexeme subrule)

subrule :: Parser (Bool, Bool, Dir, Char)
subrule = do
  _ <- lexeme "If the current value is"
  c <- bit
  _ <- lexeme ":"
  _ <- lexeme "- Write the value"
  b <- bit
  _ <- lexeme "."
  _ <- lexeme "- Move one slot to the"
  d <- dir
  _ <- lexeme "."
  _ <- lexeme "- Continue with state"
  s <- charP isAlpha
  _ <- "."
  pure (c, b, d, s)
  where
    bit = (True <$ "1") <|> (False <$ "0")
    dir = (L <$ "left") <|> (R <$ "right")

main :: IO ()
main = do
  Just prog <- parseStdin program
  print $ run prog
