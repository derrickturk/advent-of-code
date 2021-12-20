import Data.List (foldl', nub)
import qualified Data.Array as A
import qualified Data.Set as S

type LUT = A.Array Int Bool
type Image = S.Set (Int, Int)
type Pos = (Int, Int)

pixel :: Image -> Pos -> Bool
pixel img p = S.member p img

grid9 :: Pos -> [Pos]
grid9 (x, y) = [ (i, j) | j <- [y-1..y+1]
                        , i <- [x-1..x+1]
               ]

grid9Value :: Image -> Pos -> Int
grid9Value img p = bigEndian $ pixel img <$> grid9 p

bigEndian :: [Bool] -> Int
bigEndian = foldl' (\k b -> k * 2 + if b then 1 else 0) 0

parseCell :: Char -> Maybe Bool
parseCell '#' = Just True
parseCell '.' = Just False
parseCell _ = Nothing

parseLUT :: String -> Maybe LUT
parseLUT = fmap (A.listArray (0, 511)) . traverse parseCell

parseImage :: [String] -> Maybe Image
parseImage ls = do
  ls' <- traverse (traverse parseCell) ls
  Just $ S.fromList $ fmap snd $ filter fst $
    [ (v, (x, y)) | (y, row) <- zip [0..] ls'
                  , (x, v) <- zip [0..] row
                  , v
    ]

apply :: LUT -> Image -> Image
apply lut img =
  let consider = nub $ concat $ grid9 <$> S.toList img
      revised = zip consider $ (lut A.!) . grid9Value img <$> consider
      step s (p, True) = S.insert p s
      step s (p, False) = S.delete p s
   in foldl' step img revised

main :: IO ()
main = do
  Just lut <- parseLUT <$> getLine
  _ <- getLine
  Just img <- parseImage . lines <$> getContents
  let imgs = iterate (apply lut) img
  print $ S.size $ imgs !! 2
