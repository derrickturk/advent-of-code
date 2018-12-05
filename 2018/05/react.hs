import Data.Char (ord)
import Data.Bits (xor)

react1 :: [Char] -> (Bool, [Char])
react1 = go False where
  go b [] = (b, [])
  go b [c] = (b, [c])
  go b (c1:rest@(c2:cs))
    | ord c1 `xor` ord c2 == 32 = go True cs
    | otherwise = let (b', rest') = go b rest in (b', c1:rest')

react :: [Char] -> [Char]
react cs = let (b, cs') = react1 cs in if b then react cs' else cs'

main :: IO ()
main = length . react <$> getLine >>= print
