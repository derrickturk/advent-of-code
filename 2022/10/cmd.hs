import Text.Read (readMaybe)

data Cmd
  = Noop
  | AddX Int
  deriving Show

parse :: String -> Maybe Cmd
parse line = case words line of
  ["noop"] -> Just Noop
  ["addx", n] -> AddX <$> readMaybe n
  _ -> Nothing

step :: Int -> [Cmd] -> [Int]
step _ [] = []
step x (Noop:rest) = x:step x rest
step x (AddX n:rest) = x:x:step (x + n) rest

extend :: [Cmd] -> [Cmd]
extend = (<> repeat Noop)

checkpoints :: [a] -> [a]
checkpoints = take 6 . every 40 . drop 19 where
  every n xs = head xs:every n (drop n xs)

render :: [Int] -> String
render = unlines . renderLines where
  renderLines [] = []
  renderLines xs = renderLine (take 40 xs):renderLines (drop 40 xs)
  renderLine xs = renderChar <$> zip [0..] xs
  renderChar (i, x)
    | abs (i - x) <= 1 = '█'
    | otherwise = ' '

main :: IO ()
main = do
  Just cmds <- traverse parse . lines <$> getContents
  let xs = step 1 $ extend cmds
  print $ sum $ fmap (uncurry (*)) $ checkpoints $ zip [1..] xs
  putStrLn $ render $ take 240 xs
