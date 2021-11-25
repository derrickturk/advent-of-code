data State = Normal | InBruges
  deriving Show

tls :: String -> Bool
tls = go Normal False where
  go _ soFar [] = soFar
  go _ soFar ('[':rest) = go InBruges soFar rest
  go InBruges soFar (']':rest) = go Normal soFar rest
  go InBruges soFar (a:rest@(b:c:d:_))
    | a /= b && a == d && b == c = False
    | otherwise = go InBruges soFar rest
  go InBruges soFar (_:rest) = go InBruges soFar rest
  go Normal soFar (a:rest@(b:c:d:_))
    | a /= b && a == d && b == c = go Normal True rest
    | otherwise = go Normal soFar rest
  go Normal soFar _ = soFar

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ length $ filter tls input
  -- mapM_ putStrLn $ filter tls input
