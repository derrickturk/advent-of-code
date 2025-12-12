import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data Problem = Problem { height :: Int
                       , width :: Int
                       , boxes :: [Int]
                       } deriving Show

regionArea :: Problem -> Int
regionArea r = height r * width r

boxArea :: Problem -> Int
boxArea r = 9 * sum (boxes r)

parse :: String -> [Problem]
parse = mapMaybe parseLine . lines where
  parseLine l = case break (== ':') l of
    (wh, ':':' ':nums) -> do
      (w, 'x':h) <- pure $ break (== 'x') wh
      Problem <$> readMaybe w <*> readMaybe h <*> traverse readMaybe (words nums)
    _ -> Nothing

main :: IO ()
main = do
  problems <- parse <$> getContents
  print $ length $ filter (\p -> regionArea p >= boxArea p) problems
