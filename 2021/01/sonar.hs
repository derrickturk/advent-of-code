{-# LANGUAGE TypeApplications #-}

import ChunkyWindows

increases :: Ord a => [a] -> Int
increases (x:rest@(y:_))
  | y > x = 1 + increases rest
  | otherwise = increases rest
increases _ = 0

main :: IO ()
main = do
  readings <- fmap (read @Int) . lines <$> getContents
  print $ increases readings
  print $ increases $ sum <$> windows' 3 readings
