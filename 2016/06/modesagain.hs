import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.List (foldl', transpose, maximumBy, minimumBy)

unscramble :: [String] -> String
unscramble = fmap mostFrequent . transpose

unscramble2 :: [String] -> String
unscramble2 = fmap leastFrequent . transpose

count :: Ord a => [a] -> [(a, Int)]
count = M.toList . foldl' see M.empty where
  see counts x = M.insertWith (+) x 1 counts

mostFrequent :: Ord a => [a] -> a
mostFrequent = fst . maximumBy (comparing snd) . count

leastFrequent :: Ord a => [a] -> a
leastFrequent = fst . minimumBy (comparing snd) . count

main :: IO ()
main = do
  scrambled <- lines <$> getContents
  putStrLn $ unscramble scrambled
  putStrLn $ unscramble2 scrambled
