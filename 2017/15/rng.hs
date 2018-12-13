import Text.Read (readMaybe)
import Data.Int (Int16, Int32, Int64)

newtype Rng = Rng { getFactor :: Int64 }
  deriving Show

m :: Int64
m = fromIntegral (maxBound :: Int32)

stepRng :: Rng -> Int32 -> Int32
stepRng (Rng f) n = fromIntegral $ fromIntegral n * f `mod` m

genA :: Int32 -> [Int32]
genA = tail . iterate (stepRng $ Rng 16807)

genB :: Int32 -> [Int32]
genB = tail . iterate (stepRng $ Rng 48271)

genA' :: Int32 -> [Int32]
genA' = filter (\v -> v `mod` 4 == 0) . genA

genB' :: Int32 -> [Int32]
genB' = filter (\v -> v `mod` 8 == 0) . genB

matchLow :: Int32 -> Int32 -> Bool
matchLow x y = (fromIntegral x :: Int16) == fromIntegral y

parseSeed :: String -> Maybe Int32
parseSeed s = case words s of
  [_, _, _, _, n] -> readMaybe n
  _ -> Nothing

main :: IO ()
main = do
  a <- parseSeed <$> getLine
  b <- parseSeed <$> getLine
  case (,) <$> a <*> b of
    Just (a0, b0) -> do
      print $ sum $ take 40000000 $
        fromEnum <$> zipWith matchLow (genA a0) (genB b0)
      print $ sum $ take 5000000 $
        fromEnum <$> zipWith matchLow (genA' a0) (genB' b0)
    Nothing -> putStrLn "invalid input"
