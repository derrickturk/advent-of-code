{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Data.Maybe (fromJust)
import Data.List (elemIndex, foldl')
import System.IO (hPutStrLn, stderr)

import ModInv
import FemtoParsec

data Shuff
  = NewStack -- = reverse
  | Cut Int
  | Deal Int
  deriving (Show, Eq)

data Deck = Deck { start :: Int, step :: Int, size :: Int }
  deriving (Show, Eq)

shuffle :: Shuff -> Deck -> Deck
shuffle NewStack d@(Deck {..}) =
  d { start = (start + step * (size - 1)) `mod` size, step = -step }
shuffle (Cut n) d@(Deck {..}) = d { start = (start + step * n) `mod` size }
shuffle (Deal n) d@(Deck {..}) =
  let inv = fromJust $ n `modInv` size
   in d { step = (step * inv) `mod` size }

run :: [Shuff] -> Deck -> Deck
run prog deck = foldl' (\d s -> shuffle s d) deck prog

explicit :: Deck -> [Int]
explicit (Deck {..}) = take size $ iterate ((`mod` size) . (+ step)) start

shuff :: Parser Shuff
shuff =  (NewStack <$ "deal into new stack")
     <|> (Cut <$> ("cut " *> intNum))
     <|> (Deal <$> ("deal with increment " *> unsignedIntNum))

program :: Parser [Shuff]
program = some $ lexeme shuff

main :: IO ()
main = do
  prog <- parseStdin program
  case prog of
    Nothing -> hPutStrLn stderr "fail"
    Just prog' -> do
      let final = run prog' (Deck 0 1 10007)
      print $ elemIndex 2019 $ explicit final
      mapM_ print $ take 25 $ iterate (run prog') final
      {-
      let hell =
            iterate (run prog') (Deck 0 1 119315717514047) !! 101741582076661
      print $ explicit hell !! 2020
      -}
