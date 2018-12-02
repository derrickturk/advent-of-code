import Data.Foldable (foldl')
import qualified Data.Map.Strict as M

hasCounts :: Ord a => [Int] -> [a] -> [Bool]
hasCounts cs xs = fmap (not . M.null . (`M.filter` counts) . (==)) cs where
  counts = foldl' count M.empty xs
  count m c = M.insertWith (+) c 1 m

totals :: [[Bool]] -> [Int]
totals [] = repeat 0
totals (bs:bss) = zipWith (+) (fromEnum <$> bs) (totals bss)

checksum :: Ord a => [Int] -> [[a]] -> Int
checksum cs xss = product $ totals $ hasCounts cs <$> xss

main :: IO ()
main = checksum [2, 3] . words <$> getContents >>= print
