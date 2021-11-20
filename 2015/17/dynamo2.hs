-- whatever, yolo

poppinBottles :: [Int]
poppinBottles = [ 11
                , 30
                , 47
                , 31
                , 32
                , 36
                , 3
                , 1
                , 5
                , 3
                , 32
                , 36
                , 15
                , 11
                , 46
                , 26
                , 28
                , 1
                , 19
                , 3
                ]

combinations :: [a] -> [[a]]
combinations [] = [[]]
combinations (x:xs) = do
  xs' <- combinations xs
  [x:xs', xs']

main :: IO ()
main = do
  let good = filter ((== 150) . sum) $ combinations poppinBottles
      minLength = minimum $ length <$> good
      goodOfMinLength = filter ((== minLength) . length) good
  print $ length good
  print $ length goodOfMinLength
