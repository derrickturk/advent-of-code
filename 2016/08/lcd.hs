{-# LANGUAGE OverloadedStrings #-}

import Data.Array
import Data.List (foldl')

import FemtoParsec

newtype Screen = Screen { getScreen :: Array (Int, Int) Bool }
  deriving Show

mkScreen :: Int -> Int -> Screen
mkScreen y x = Screen $ listArray ((0, 0), (y - 1, x - 1)) $ repeat False

rowLists :: Screen -> [[Bool]]
rowLists (Screen a) = do
  let ((i0, j0), (i1, j1)) = bounds a
  i <- [i0..i1]
  pure [a ! (i, j) | j <- [j0..j1]]

lit :: Screen -> Int
lit (Screen a) = length $ filter id $ elems a

display :: Screen -> IO ()
display = mapM_ printRow . rowLists where
  printRow = putStrLn . fmap printCell
  printCell True = '#'
  printCell False = '.'

data Command
  = Rect Int Int
  | RotateRow Int Int
  | RotateCol Int Int
  deriving Show

run :: Screen -> Command -> Screen
run (Screen a) (Rect x y) = Screen $
  a // [((i, j), True) | i <- [0..y - 1], j <- [0..x - 1]]
run (Screen a) (RotateRow y n) =
  let ((_, _), (_, j1)) = bounds a
      cols = j1 + 1
      updates = [((y, (j + n) `mod` cols), a ! (y, j)) | j <- [0..j1]]
   in Screen $ a // updates
run (Screen a) (RotateCol x n) =
  let ((_, _), (i1, _)) = bounds a
      rows = i1 + 1
      updates = [(((i + n) `mod` rows, x), a ! (i, x)) | i <- [0..i1]]
   in Screen $ a // updates

command :: Parser Command
command = rect <|> rotateRow <|> rotateCol where
  rect = Rect <$> ("rect " *> unsignedIntNum) <*> ("x" *> unsignedIntNum)
  rotateRow = RotateRow <$> ("rotate row y=" *> unsignedIntNum)
                        <*> (" by " *> unsignedIntNum)
  rotateCol = RotateCol <$> ("rotate column x=" *> unsignedIntNum)
                        <*> (" by " *> unsignedIntNum)

main :: IO ()
main = do
  Just commands <- parseStdin $ many $ lexeme command
  let final = foldl' run (mkScreen 6 50) commands
  display final
  print $ lit final
