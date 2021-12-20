import Data.List (foldl', nub)
import qualified Data.Array as A
import qualified Data.Map.Strict as M

type LUT = A.Array Int Bool
type Image = (M.Map (Int, Int) Bool, Bool)
type Pos = (Int, Int)

pixel :: Image -> Pos -> Bool
pixel (m, space) p = maybe space id $ m M.!? p

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
  Just $ (M.fromList $
    [ ((x, y), v) | (y, row) <- zip [0..] ls'
                  , (x, v) <- zip [0..] row
                  , v
    ],
    False)

apply :: LUT -> Image -> Image
apply lut img@(m, space) =
  let consider = nub $ concat $ grid9 <$> M.keys m
      revised = zip consider $ (lut A.!) . grid9Value img <$> consider
      step m' (p, b) = M.insert p b m'
      space' = if space then lut A.! 511 else lut A.! 0
   in (foldl' step m revised, space')

lit :: Image -> Maybe Int
lit (_, True) = Nothing
lit (m, False) = Just $ length $ filter id $ M.elems m

main :: IO ()
main = do
  Just lut <- parseLUT <$> getLine
  _ <- getLine
  Just img <- parseImage . lines <$> getContents
  let imgs = iterate (apply lut) img
  print $ lit $ imgs !! 2
  print $ lit $ imgs !! 50
