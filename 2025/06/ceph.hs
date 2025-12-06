import Control.Applicative (ZipList(..), getZipList)
import Data.List (unsnoc)
import Text.Read (readMaybe)

data Op = Mul | Add deriving Show
data Ceph = Ceph [Int] Op deriving Show

transpose :: [[a]] -> [[a]]
transpose = getZipList . traverse ZipList

parse :: String -> Maybe [Ceph]
parse = traverse parseCeph . transpose . fmap words . lines where
  parseCeph ws = do
    (nums, op) <- unsnoc ws
    nums' <- traverse readMaybe nums
    op' <- case op of
      "+" -> Just Add
      "*" -> Just Mul
      _ -> Nothing
    pure $ Ceph nums' op'

eval :: Ceph -> Int
eval (Ceph nums Add) = sum nums
eval (Ceph nums Mul) = product nums

main :: IO ()
main = do
  Just cephs <- parse <$> getContents
  print $ sum $ eval <$> cephs
