import Control.Monad (guard)

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

abas :: String -> [String]
abas = go Normal where
  go _ [] = []
  go Normal ('[':rest) = go InBruges rest
  go Normal (a:rest@(b:c:_))
    | a /= b && a == c = (a:b:c:[]):go Normal rest
    | otherwise = go Normal rest
  go InBruges (']':rest) = go Normal rest
  go st (_:rest) = go st rest

babs :: String -> [String]
babs = go Normal where
  go _ [] = []
  go Normal ('[':rest) = go InBruges rest
  go InBruges (']':rest) = go Normal rest
  go InBruges (a:rest@(b:c:_))
    | a /= b && a == c = (a:b:c:[]):go InBruges rest
    | otherwise = go InBruges rest
  go st (_:rest) = go st rest

ssl :: String -> Bool
ssl addr = not $ null $ do
  [a, b, _] <- abas addr
  [c, d, _] <- babs addr
  guard $ a == d && b == c
  pure (a, b)

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ length $ filter tls input
  print $ length $ filter ssl input
