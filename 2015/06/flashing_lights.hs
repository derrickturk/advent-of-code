{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (foldl')
import Data.Char (isSpace)
import Text.Read (readMaybe)

class Griddy a where
  unlit :: a
  turnOn :: (Int, Int) -> a -> a
  turnOff :: (Int, Int) -> a -> a
  toggle :: (Int, Int) -> a -> a

newtype BasicGrid = BasicGrid { getBasicGrid :: S.Set (Int, Int) }
  deriving (Eq, Show)

instance Griddy BasicGrid where
  unlit = BasicGrid S.empty

  turnOn c (BasicGrid s) = BasicGrid $ S.insert c s

  turnOff c (BasicGrid s) = BasicGrid $ S.delete c s

  toggle c g@(BasicGrid s) = if S.member c s
    then turnOff c g
    else turnOn c g

countLit :: BasicGrid -> Int
countLit = S.size . getBasicGrid

newtype FancyGrid = FancyGrid { getFancyGrid :: M.Map (Int, Int) Int }
  deriving (Eq, Show)

instance Griddy FancyGrid where
  unlit = FancyGrid M.empty

  turnOn c (FancyGrid m) = FancyGrid $ M.alter f c m where
    f Nothing = Just 1
    f (Just n) = Just (n + 1)

  turnOff c (FancyGrid m) = FancyGrid $ M.alter f c m where
    f Nothing = Nothing
    f (Just n)
      | n > 1 = Just (n - 1)
      | otherwise = Nothing

  toggle c (FancyGrid m) = FancyGrid $ M.alter f c m where
    f Nothing = Just 2
    f (Just n) = Just (n + 2)

countLumens :: FancyGrid -> Integer
countLumens = sum . fmap fromIntegral . M.elems . getFancyGrid

data Instruction
  = TurnOn (Int, Int) (Int, Int)
  | TurnOff (Int, Int) (Int, Int)
  | Toggle (Int, Int) (Int, Int)
  deriving (Eq, Show)

range :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
range (x0, y0) (x1, y1) = [(x, y) | x <- [x0..x1], y <- [y0..y1]]

apply :: Griddy g => Instruction -> g -> g
apply (TurnOn from to) = foldGridOp turnOn (range from to)
apply (TurnOff from to) = foldGridOp turnOff (range from to)
apply (Toggle from to) = foldGridOp toggle (range from to)

foldGridOp :: Griddy g => ((Int, Int) -> g -> g) -> [(Int, Int)] -> g -> g
foldGridOp op coords init = foldl' step init coords where
  step g c = op c g

applyAll :: Griddy g => [Instruction] -> g -> g
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
  print $ countLumens $ applyAll instrs unlit
