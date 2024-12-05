import Data.Char (isDigit)
import Data.List (span)

data Op
  = Mul Int Int
  | Do
  | Dont
  deriving Show

eval1 :: [Op] -> Int
eval1 [] = 0
eval1 (Mul x y:rest) = x * y + eval1 rest
eval1 (_:rest) = eval1 rest

eval2 :: [Op] -> Int
eval2 = go True where
  go _ [] = 0
  go _ (Do:rest) = go True rest
  go _ (Dont:rest) = go False rest
  go True (Mul x y:rest) = x * y + go True rest
  go False (_:rest) = go False rest

parse :: String -> [Op]
parse "" = []
parse ('m':'u':'l':'(':rest) =
  case span isDigit rest of
    ("", _) -> parse rest
    (num1, ',':rest') -> case span isDigit rest' of
      ("", _) -> parse rest'
      (num2, ')':rest'') -> Mul (read num1) (read num2) : parse rest''
      (_, rest'') -> parse rest''
    (_, rest') -> parse rest'
parse ('d':'o':'(':')':rest) = Do : parse rest
parse ('d':'o':'n':'\'':'t':'(':')':rest) = Dont : parse rest
parse (_:rest) = parse rest

main :: IO ()
main = do
  code <- parse <$> getContents
  print $ eval1 code
  print $ eval2 code
