import Control.Applicative (ZipList(..), getZipList)
import Data.List (unsnoc)
import Text.Read (readMaybe)

data Op = Mul | Add deriving Show
data Ceph = Ceph [Int] Op deriving Show

eval :: Ceph -> Int
eval (Ceph nums Add) = sum nums
eval (Ceph nums Mul) = product nums

transpose :: [[a]] -> [[a]]
transpose = getZipList . traverse ZipList

parseOp :: String -> Maybe Op
parseOp "+" = Just Add
parseOp "*" = Just Mul
parseOp _ = Nothing

parse :: String -> Maybe [Ceph]
parse = traverse parseCeph . transpose . fmap words . lines where
  parseCeph ws = do
    (nums, op) <- unsnoc ws
    Ceph <$> traverse readMaybe nums <*> parseOp op

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split p xs = case break p xs of
  (pre, []) -> [pre]
  (pre, _:suf) -> pre:split p suf

parse' :: String -> Maybe [Ceph]
parse' = traverse parseBlock . split (all (== ' ')) . transpose . lines where
  parseBlock [] = Nothing
  parseBlock (first:rest) = do
    (num, op) <- unsnoc first
    Ceph <$> traverse readMaybe (num:rest) <*> parseOp [op]

main :: IO ()
main = do
  contents <- getContents
  Just cephs <- pure $ parse contents
  Just cephs' <- pure $ parse' contents
  print $ sum $ eval <$> cephs
  print $ sum $ eval <$> cephs'
