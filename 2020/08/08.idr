import Data.Either
import Data.List
import Data.SortedSet
import Data.Strings
import Data.Vect
import System.File

data Op
  = Nop
  | Acc
  | Jmp

data Instr = MkInstr Op Integer

Program : Nat -> Type
Program n = Vect n Instr

record GameBoiii n where
  constructor MkGameBoiii
  program : Program n
  ip : Fin n
  acc : Integer

gameBoiii : Program (S k) -> GameBoiii (S k)
gameBoiii prog = MkGameBoiii prog FZ 0

data GameBoiiiError
  = ParseError String
  | SegFault Integer

safeStepIp : {n : _} -> Fin n -> Maybe (Fin n)
safeStepIp = getRight . strengthen . FS

safeJmpIp : {n : Nat} -> Fin n -> Integer -> Either GameBoiiiError (Maybe (Fin n))
safeJmpIp ip val =
  let ip' = finToInteger ip + val
   in case integerToFin ip' (S n) of
     Nothing => Left (SegFault ip')
     Just k => Right $ getRight $ strengthen $ k

step : {n : Nat} -> GameBoiii n -> Either GameBoiiiError (Maybe (GameBoiii n))
step (MkGameBoiii program ip acc) with (index ip program)
  step (MkGameBoiii program ip acc) | (MkInstr Nop val) =
    Right (MkGameBoiii program <$> safeStepIp ip <*> pure acc)
  step (MkGameBoiii program ip acc) | (MkInstr Acc val) =
    Right (MkGameBoiii program <$> safeStepIp ip <*> pure (acc + val))
  step (MkGameBoiii program ip acc) | (MkInstr Jmp val) =
    case safeJmpIp ip val of
      Left e => Left e
      Right Nothing => Right Nothing
      Right (Just ip) => Right (Just (MkGameBoiii program ip acc))

parseInstr : String -> Either GameBoiiiError Instr
parseInstr line = case words line of
  [op, val] => case (op, parseInteger val) of
    ("nop", Just n) => Right (MkInstr Nop n)
    ("acc", Just n) => Right (MkInstr Acc n)
    ("jmp", Just n) => Right (MkInstr Jmp n)
    _ => Left (ParseError line) 
  _ => Left (ParseError line)

parseProgram : List String -> Either GameBoiiiError (n ** Vect n Instr)
parseProgram lines = do
  instrs <- traverse parseInstr $ filter (/= "") lines -- wtf getLines
  Right (length instrs ** fromList instrs)

getLines : IO (List String)
getLines = do
  eof <- fEOF stdin
  if eof
    then pure []
    else do
      (::) <$> getLine <*> getLines

runGameBoiii' : {n : _} -> SortedSet (Fin n) -> GameBoiii n -> IO ()
runGameBoiii' s g = case step g of
  Left (ParseError l) => putStrLn $ "parse error: " <+> l
  Left (SegFault ip) => putStrLn $ "segfault @ " <+> show ip
  Right Nothing => putStrLn "<HALT>"
  Right (Just g') => if contains g'.ip s
    then putStrLn $ "loop @ ip = " <+> show (finToInteger g'.ip)
      <+> ", acc = " <+> show g'.acc
    else runGameBoiii' (insert g.ip s) g'

runGameBoiii : {n : _} -> GameBoiii n -> IO ()
runGameBoiii = runGameBoiii' empty

main : IO ()
main = do
  res <- parseProgram <$> getLines
  case res of
    Left (ParseError l) => putStrLn $ "parse error: " <+> l
    Left (SegFault ip) => putStrLn $ "segfault @ " <+> show ip
    Right (S k ** prog) => runGameBoiii (gameBoiii prog)
    Right (Z ** _) => putStrLn $ "empty program"
