{-# LANGUAGE OverloadedStrings, TypeApplications #-}

import Data.List (elemIndex, sort)

import FemtoParsec

data Packet
  = One Int
  | Many [Packet]
  deriving (Eq, Show)

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (One m) (One n) = compare m n
  compare (One m) ns = compare (Many [One m]) ns
  compare ms (One n) = compare ms (Many [One n])
  compare (Many ms) (Many ns) = compare ms ns

packetP :: Parser Packet
packetP =  One <$> unsignedIntNum
       <|> Many [] <$ "[]"
       <|> Many <$> ("[" *> sepBy "," packetP <* "]")

packetPairs :: Parser [(Packet, Packet)]
packetPairs = some $ (,) <$> lexeme packetP <*> lexeme packetP

sortAll :: [(Packet, Packet)] -> [Packet]
sortAll = sort . concatMap (\(a, b) -> [a, b])

main :: IO ()
main = do
  Just pairs <- parseStdin packetPairs
  let dividers = (Many [Many [One 2]], Many [Many [One 6]])
      sorted = sortAll $ dividers:pairs
  print $ sum @_ @Int $
    fst <$> filter (\(_, (a, b)) -> compare a b == LT) (zip [1..] pairs)
  Just i1 <- pure $ elemIndex (fst dividers) sorted
  Just i2 <- pure $ elemIndex (snd dividers) sorted
  print $ (i1 + 1) * (i2 + 1)
