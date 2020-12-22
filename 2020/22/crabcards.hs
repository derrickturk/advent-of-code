import Text.Read (readMaybe)

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
  case uncurry playGame decks of
    Left self -> do
      putStrLn "self wins!"
      print $ self
      print $ score self
    Right crab -> do
      putStrLn "crab wins!"
      print $ crab
      print $ score crab
