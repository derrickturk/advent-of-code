import Text.Read (readMaybe)
import Data.Char (isDigit)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S

parseLine :: String -> Maybe (Int, [Int])
parseLine s = case words s of
  (from:_:tos) -> do
    from' <- readMaybe from
    tos' <- traverse readMaybe $ takeWhile isDigit <$> tos
    pure (from', tos')
  _ -> Nothing

parseMap :: String -> Maybe (M.Map Int [Int])
parseMap = fmap M.fromList . traverse parseLine . lines

connectedGroup :: M.Map Int [Int] -> Int -> S.Set Int
connectedGroup m n = connectedGroup' m S.empty n where
  connectedGroup' _ s n
    | S.member n s = s
    | otherwise = foldl' (connectedGroup' m)
                         (S.insert n s)
                         (M.findWithDefault [] n m)

groups :: M.Map Int [Int] -> [S.Set Int]
groups m = snd $ foldr checkGroup (S.empty, []) $ M.keys m where
  checkGroup n (s, gs)
    | S.member n s = (s, gs)
    | otherwise = let g = connectedGroup m n in (S.union s g, g:gs)

main :: IO ()
main = do
  input <- parseMap <$> getContents
  case input of
    Just map -> do
      print $ S.size $ connectedGroup map 0
      print $ length $ groups map
    Nothing -> putStrLn "invalid input"
