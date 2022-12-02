import System.IO (hPutStrLn, stderr)

data RPS = Rock | Paper | Scissors deriving (Show, Eq, Enum)

beat :: RPS -> RPS
beat = toEnum . (`mod` 3) . (+ 1) . fromEnum

draw :: RPS -> RPS
draw = id

lose :: RPS -> RPS
lose = toEnum . (`mod` 3) . (subtract 1) . fromEnum

score :: RPS -> RPS -> Int
score they me
  | they == beat me = 0 + fromEnum me + 1
  | they == lose me = 6 + fromEnum me + 1
  | otherwise = 3 + fromEnum me + 1

parseLine :: String -> Maybe (RPS, RPS, RPS)
parseLine l = case words l of 
  [they, me] -> do
    they' <- parseABC they
    me' <- parseXYZ me
    me2 <- parseXYZ2 me they'
    pure (they', me', me2)
  _ -> Nothing
  where
    parseABC "A" = Just Rock
    parseABC "B" = Just Paper
    parseABC "C" = Just Scissors
    parseABC _ = Nothing
    parseXYZ "X" = Just Rock
    parseXYZ "Y" = Just Paper
    parseXYZ "Z" = Just Scissors
    parseXYZ _ = Nothing
    parseXYZ2 "X" they = Just (lose they)
    parseXYZ2 "Y" they = Just (draw they)
    parseXYZ2 "Z" they = Just (beat they)
    parseXYZ2 _ _ = Nothing

main :: IO ()
main = do
  rounds <- traverse parseLine . lines <$> getContents
  case rounds of
    Just rounds' -> do
      print $ sum $ (\(they, me, _) -> score they me) <$> rounds'
      print $ sum $ (\(they, _, me) -> score they me) <$> rounds'
    _ -> hPutStrLn stderr "bad input"
