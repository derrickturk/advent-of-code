import Data.Maybe (catMaybes)
import Data.List (foldl', sort)
import Data.Tuple (swap)

delims :: [(Char, Char)]
delims = [ ('(', ')')
         , ('[', ']')
         , ('{', '}')
         , ('<', '>')
         ]

delims' :: [(Char, Char)]
delims' = swap <$> delims

opener :: Char -> Bool
opener c = elem c $ fst <$> delims

closer :: Char -> Bool
closer c = elem c $ snd <$> delims

firstNaughty :: String -> Maybe Char
firstNaughty = go [] where
  go _ [] = Nothing
  go [] (c:rest)
    | opener c = go [c] rest
    | otherwise = Just c
  go stack@(o:os) (c:rest)
    | opener c = go (c:stack) rest
    | closer c = case lookup c delims' of
        Just o' -> if o == o'
                     then go os rest
                     else Just c
        _ -> error "that shouldn't happen"
    | otherwise = Just c -- ???

repair :: String -> String
repair = go [] where
  go os [] = catMaybes $ (`lookup` delims) <$> os
  go [] (c:rest)
    | opener c = go [c] rest
    | otherwise = go [] rest
  go stack@(o:os) (c:rest)
    | opener c = go (c:stack) rest
    | closer c = case lookup c delims' of
        Just o' -> if o == o'
                     then go os rest
                     else go stack rest
        _ -> error "that shouldn't happen"
    | otherwise = go stack rest

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
score _ = 0

repairScore :: Char -> Int
repairScore ')' = 1
repairScore ']' = 2
repairScore '}' = 3
repairScore '>' = 4
repairScore _ = 0

totalRepairScore :: String -> Int
totalRepairScore = foldl' seeChar 0 where
  seeChar tot c = tot * 5 + repairScore c

median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` 2)

main :: IO ()
main = do
  inputs <- lines <$> getContents
  let maybeCorrupt = firstNaughty <$> inputs
      naughty = catMaybes maybeCorrupt
      nice = fst <$> filter ((== Nothing) . snd) (zip inputs maybeCorrupt)
  print $ sum $ score <$> naughty
  print $ median $ (totalRepairScore . repair) <$> nice
