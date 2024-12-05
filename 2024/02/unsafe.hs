import Text.Read (readMaybe)

type Report = [Int]

parseReport :: String -> Maybe Report
parseReport = traverse readMaybe . words

deltas :: Report -> [Int]
deltas (x:rest@(y:_)) = y - x:deltas rest
deltas _ = []

safe :: Report -> Bool
safe xs =
  let ds = deltas xs
   in all (> 0) ds && all (< 4) ds || all (< 0) ds && all (> (-4)) ds

leaveOneOut :: [a] -> [[a]]
leaveOneOut [] = []
leaveOneOut (x:xs) = xs:[x:ys | ys <- leaveOneOut xs]

safeish :: Report -> Bool
safeish xs = safe xs || any safe (leaveOneOut xs)

main :: IO ()
main = do
  Just reports <- traverse parseReport . lines <$> getContents
  print $ length $ filter safe reports
  print $ length $ filter safeish reports
