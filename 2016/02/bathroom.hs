import Prelude hiding (Left, Right)
import Data.List (scanl', foldl')

data Direction = Up | Down | Left | Right
  deriving (Eq, Show)

move :: Int -> Direction -> Int
move n Up
  | n < 4 = n
  | otherwise = n - 3
move n Down
  | n > 6 = n
  | otherwise = n + 3
move n Left
  | n `mod` 3 == 1 = n
  | otherwise = n - 1
move n Right
  | n `mod` 3 == 0 = n
  | otherwise = n + 1

-- IDK, there's probably a pattern
move' :: Int -> Direction -> Int
move' 1 Down = 3
move' 2 Down = 6
move' 2 Right = 3
move' 3 Up = 1
move' 3 Down = 7
move' 3 Left = 2
move' 3 Right = 4
move' 4 Down = 8
move' 4 Left = 3
move' 5 Right = 6
move' 6 Up = 2
move' 6 Down = 0xa
move' 6 Left = 5
move' 6 Right = 7
move' 7 Up = 3
move' 7 Down = 0xb
move' 7 Left = 6
move' 7 Right = 8
move' 8 Up = 4
move' 8 Down = 0xc
move' 8 Left = 7
move' 8 Right = 9
move' 9 Left = 8
move' 0xa Up = 6
move' 0xa Right = 0xb
move' 0xb Up = 7
move' 0xb Down = 0xd
move' 0xb Left = 0xa
move' 0xb Right = 0xc
move' 0xc Up = 8
move' 0xc Left = 0xb
move' 0xd Up = 0xb
move' n _ = n

parseDirection :: Char -> Maybe Direction
parseDirection 'U' = Just Up -- for a snack
parseDirection 'D' = Just Down -- for whatever
parseDirection 'L' = Just Left -- to pick up some cigarettes
parseDirection 'R' = Just Right -- there
parseDirection _ = Nothing 

parseDigitMoves :: String -> Maybe [Direction]
parseDigitMoves = traverse parseDirection

padChar :: Int -> Char
padChar n
  | n >= 0 && n <= 9 = toEnum $ n + fromEnum '0'
  | n >= 10 && n <= 15 = toEnum $ n - 10 + fromEnum 'A'
  | otherwise = '!'

main :: IO ()
main = do
  Just prog <- traverse parseDigitMoves . lines <$> getContents
  putStrLn $ fmap padChar $ tail $ scanl' (foldl' move) 5 prog
  putStrLn $ fmap padChar $ tail $ scanl' (foldl' move') 5 prog
