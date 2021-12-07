{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)

type Crabs = [Int]

escalateCost :: Int -> Int
escalateCost n = sum [1..n]

balanceCrabs :: Crabs -> (Int, Int)
balanceCrabs cs = minimum $ balance <$> [minimum cs..maximum cs] where
  balance i = (sum $ (\c -> abs (c - i)) <$> cs, i)

balanceCrabs2 :: Crabs -> (Int, Int)
balanceCrabs2 cs = minimum $ balance <$> [minimum cs..maximum cs] where
  balance i = (sum $ (\c -> escalateCost $ abs (c - i)) <$> cs, i)

parseCrabs :: T.Text -> Maybe Crabs
parseCrabs = traverse (readMaybe . T.unpack) . T.splitOn ","

main :: IO ()
main = do
  Just crabs <- parseCrabs <$> TIO.getLine
  print $ balanceCrabs crabs
  print $ balanceCrabs2 crabs
