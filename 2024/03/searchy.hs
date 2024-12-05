import Data.Char (isDigit)
import Data.List (span)

data Mul = Mul Int Int
  deriving Show

eval :: Mul -> Int
eval (Mul x y) = x * y

muls :: String -> [Mul]
muls "" = []
muls ('m':'u':'l':'(':rest) =
  case span isDigit rest of
    ("", _) -> muls rest
    (num1, ',':rest') -> case span isDigit rest' of
      ("", _) -> muls rest'
      (num2, ')':rest'') -> Mul (read num1) (read num2) : muls rest''
      (_, rest'') -> muls rest''
    (_, rest') -> muls rest'
muls (_:rest) = muls rest

main :: IO ()
main = do
  code <- muls <$> getContents
  print code
  print $ sum $ eval <$> code
