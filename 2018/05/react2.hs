import Data.Char (ord, toUpper)
import Data.Bits (xor)
import Data.List (minimumBy)

react1 :: [Char] -> (Bool, [Char])
react1 = go False where
  go b [] = (b, [])
  go b [c] = (b, [c])
  go b (c1:rest@(c2:cs))
    | ord c1 `xor` ord c2 == 32 = go True cs
    | otherwise = let (b', rest') = go b rest in (b', c1:rest')

react :: [Char] -> [Char]
react cs = let (b, cs') = react1 cs in if b then react cs' else cs'

best :: [Char] -> (Char, Int)
best cs = minimumBy (\(_, n1) (_, n2) -> compare n1 n2) $
  fmap (without cs) ['a'..'z']
  where
    without cs c = (c, length $ react $ filter (notChar c) cs)
    notChar c c' = c /= c' && toUpper c /= c'

main :: IO ()
main = snd . best <$> getLine >>= print
