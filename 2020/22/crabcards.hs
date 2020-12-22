import Text.Read (readMaybe)
import qualified Data.Set as S

-- crab wins ties!
playRound :: [Integer] -> [Integer] -> ([Integer], [Integer])
playRound [] crab = ([], crab)
playRound self [] = (self, [])
playRound (x:self) (y:crab)
  | x > y = (self <> [x, y], crab)
  | otherwise = (self, crab <> [y, x])

-- crab is always right
playGame :: [Integer] -> [Integer] -> Either [Integer] [Integer]
playGame self crab = case playRound self crab of
  (self', []) -> Left self'
  ([], crab') -> Right crab'
  (self', crab') -> playGame self' crab'

type GameStateMemo = S.Set ([Integer], [Integer])

takeExact :: Integer -> [a] -> Maybe [a]
takeExact 0 _ = Just []
takeExact _ [] = Nothing
takeExact n (x:xs) = (x:) <$> takeExact (n - 1) xs

playRoundRecursive :: GameStateMemo
              -> [Integer]
              -> [Integer]
              -> (GameStateMemo, [Integer], [Integer])
playRoundRecursive memo self crab
  | S.member (self, crab) memo = (memo, self, []) -- self wins (destroy crab)
  | otherwise =
      let (self'', crab'') = case (self, crab) of
            (self', []) -> (self', [])
            ([], crab') -> ([], crab')
            ((x:self'), (y:crab')) -> case (takeExact x self', takeExact y crab') of
              (Just recSelf, Just recCrab) -> case playGameRecursive recSelf recCrab of
                Left _ -> (self' <> [x, y], crab')
                Right _ -> (self', crab' <> [y, x])
              (_, _) -> if x > y
                then (self' <> [x, y], crab')
                else (self', crab' <> [y, x])
       in (S.insert (self, crab) memo, self'', crab'')

playGameRecursive :: [Integer] -> [Integer] -> Either [Integer] [Integer]
playGameRecursive self crab = go S.empty self crab where
  go memo s c = case playRoundRecursive memo s c of
    (_, self', []) -> Left self'
    (_, [], crab') -> Right crab'
    (memo', self', crab') -> go memo' self' crab'

score :: [Integer] -> Integer
score cards = go 0 (fromIntegral $ length cards) cards where
  go thusFar _ [] = thusFar
  go thusFar mult (x:xs) = go (mult * x + thusFar) (mult - 1) xs

parse :: [String] -> ([Integer], [Integer])
parse inputLines = (p1, p2) where
  (p1, p2lines) = parse1 inputLines
  p2 = parse2 p2lines
  parse1 [] = ([], [])
  parse1 ("Player 1:":rest) = parse1 rest
  parse1 (x:rest) = case readMaybe x of
    Just x' -> (\(xs, r) -> ((x':xs), r)) $ parse1 rest
    Nothing -> ([], rest)
  parse2 [] = []
  parse2 ("Player 2:":rest) = parse2 rest
  parse2 (x:rest) = case readMaybe x of
    Just x' -> x':parse2 rest
    Nothing -> []

main :: IO ()
main = do
  decks <- parse . lines <$> getContents

  putStrLn "ORDINARY KOMBAT!" 
  case uncurry playGame decks of
    Left self -> do
      putStrLn "self wins!"
      print $ self
      print $ score self
    Right crab -> do
      putStrLn "crab wins!"
      print $ crab
      print $ score crab

  putStrLn "RECURSIVE KOMBAT!" 
  case uncurry playGameRecursive decks of
    Left self -> do
      putStrLn "self wins!"
      print $ self
      print $ score self
    Right crab -> do
      putStrLn "crab wins!"
      print $ crab
      print $ score crab
