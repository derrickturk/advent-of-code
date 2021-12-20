import Data.List (foldl')
import qualified Data.Array as A
import qualified Data.Set as S

type LUT = A.Array Int Bool
-- set of non-void-color pixels, void color
type Image = (S.Set (Int, Int), Bool)
type Pos = (Int, Int)

pixel :: Image -> Pos -> Bool
pixel (s, void) p = if S.member p s then not void else void

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
  Just $ (S.fromList $
    [ (x, y) | (y, row) <- zip [0..] ls'
             , (x, v) <- zip [0..] row
             , v
    ],
    False)

-- a faster nub
unique :: Ord a => [a] -> [a]
unique = S.toList . S.fromList

apply :: LUT -> Image -> Image
apply lut img@(s, void) =
  let consider = unique $ concat $ grid9 <$> S.toList s
      revised = zip consider $ (lut A.!) . grid9Value img <$> consider
      void' = if void then lut A.! 511 else lut A.! 0
      step s' (p, b) = if b /= void' then S.insert p s' else s'
   in (foldl' step S.empty revised, void')

lit :: Image -> Maybe Int
lit (_, True) = Nothing
lit (s, False) = Just $ S.size s

main :: IO ()
main = do
  Just lut <- parseLUT <$> getLine
  _ <- getLine
  Just img <- parseImage . lines <$> getContents
  let imgs = iterate (apply lut) img
  print $ lit $ imgs !! 2
  print $ lit $ imgs !! 50
