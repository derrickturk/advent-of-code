{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Data.Array
import FemtoParsec

data Register
  = A
  | B
  deriving Show

data Instruction
  = Half Register
  | Triple Register
  | Increment Register
  | Jump Int
  | JumpEven Register Int
  | JumpOne Register Int
  deriving Show

data Computer
  = Computer { instructions :: Array Int Instruction
             , ip :: Int
             , regA :: Integer
             , regB :: Integer
             } deriving Show

computer :: Array Int Instruction -> Computer
computer prog = Computer prog 0 0 0

step :: Computer -> Maybe Computer
step comp
  | inRange (bounds $ instructions comp) (ip comp) =
      Just $ stepI (instructions comp ! ip comp) comp
  | otherwise = Nothing
  where
    stepI :: Instruction -> Computer -> Computer
    stepI (Half reg) = next . modifyR reg (`div` 2)
    stepI (Triple reg) = next . modifyR reg (* 3)
    stepI (Increment reg) = next . modifyR reg (+ 1)
    stepI (Jump n) = \c -> c { ip = ip c + n }
    stepI (JumpEven reg n) = \c -> if even (getR reg c)
      then c { ip = ip c + n }
      else next c
    stepI (JumpOne reg n) = \c -> if getR reg c == 1
      then c { ip = ip c + n }
      else next c

    getR A = regA
    getR B = regB

    modifyR A f c = c { regA = f (regA c) }
    modifyR B f c = c { regB = f (regB c) }

    next c = c { ip = ip c + 1 }

toArray :: [a] -> Array Int a
toArray xs = listArray (0, length xs - 1) xs

register :: Parser Register
register = (A <$ "a") <|> (B <$ "b")

offset :: Parser Int
offset = fromIntegral <$> integer

instruction :: Parser Instruction
instruction =  Half <$> (lexeme "hlf" *> register)
           <|> Triple <$> (lexeme "tpl" *> register)
           <|> Increment <$> (lexeme "inc" *> register)
           <|> Jump <$> (lexeme "jmp" *> offset)
           <|> JumpEven <$> (lexeme "jie" *> register <* lexeme ",")
                        <*> offset
           <|> JumpOne <$> (lexeme "jio" *> register <* lexeme ",")
                       <*> offset

program :: Parser (Array Int Instruction)
program = toArray <$> many (lexeme instruction)

run :: Computer -> Computer
run comp = case step comp of
  Just comp' -> run comp'
  Nothing -> comp

trace :: Computer -> [Computer]
trace comp = comp:case step comp of
  Just comp' -> trace comp'
  Nothing -> []

main :: IO ()
main = do
  Just prog <- parseStdin program
  let comp = computer prog
  print $ regB $ run comp
  print $ regB $ run $ comp { regA = 1 }
