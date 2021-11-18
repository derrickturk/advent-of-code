{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (foldl')
import Data.Char (isSpace)
import Text.Read (readMaybe)

newtype Grid = Grid { getGrid :: S.Set (Int, Int) }
  deriving (Eq, Show)

unlit :: Grid
unlit = Grid S.empty

turnOn :: (Int, Int) -> Grid -> Grid
turnOn c (Grid s) = Grid $ S.insert c s

turnOff :: (Int, Int) -> Grid -> Grid
turnOff c (Grid s) = Grid $ S.delete c s

toggle :: (Int, Int) -> Grid -> Grid
toggle c g@(Grid s) = if S.member c s
  then turnOff c g
  else turnOn c g

countLit :: Grid -> Int
countLit = S.size . getGrid

data Instruction
  = TurnOn (Int, Int) (Int, Int)
  | TurnOff (Int, Int) (Int, Int)
  | Toggle (Int, Int) (Int, Int)
  deriving (Eq, Show)

range :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
range (x0, y0) (x1, y1) = [(x, y) | x <- [x0..x1], y <- [y0..y1]]

apply :: Instruction -> Grid -> Grid
apply (TurnOn from to) = foldGridOp turnOn (range from to)
apply (TurnOff from to) = foldGridOp turnOff (range from to)
apply (Toggle from to) = foldGridOp toggle (range from to)

foldGridOp :: ((Int, Int) -> Grid -> Grid) -> [(Int, Int)] -> Grid -> Grid
foldGridOp op coords init = foldl' step init coords where
  step g c = op c g

applyAll :: [Instruction] -> Grid -> Grid
applyAll instrs init = foldl' (flip apply) init instrs

parseInstruction :: T.Text -> Maybe Instruction
parseInstruction line = case T.split isSpace line of
  ["turn", "on", from, "through", to] -> TurnOn <$> coord from <*> coord to
  ["turn", "off", from, "through", to] -> TurnOff <$> coord from <*> coord to
  ["toggle", from, "through", to] -> Toggle <$> coord from <*> coord to
  _ -> Nothing
  where
    coord str = case T.split (== ',') str of
      [x, y] -> (,) <$> readMaybe (T.unpack x) <*> readMaybe (T.unpack y)
      _ -> Nothing

main :: IO ()
main = do
  Just instrs <- traverse parseInstruction . T.lines <$> TIO.getContents
  print $ countLit $ applyAll instrs unlit
