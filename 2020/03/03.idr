import Data.SortedSet
import Data.Strings
import System.File

import Data.List as L 
import Data.SortedSet as S

record Forest where
  constructor MkForest
  width, height : Int
  trees : SortedSet (Int, Int)

-- gross, this sucks
map : Ord b => (a -> b) -> SortedSet a -> SortedSet b
map f = fromList . map f . S.toList

Semigroup Forest where
  f1 <+> f2 =
    MkForest (max f1.width f2.width) (f1.height + f2.height)
      (f1.trees `union` map (\(x, y) => (x, y + f1.height)) f2.trees)

Monoid Forest where
  neutral = MkForest 0 0 empty

parseForestRow : String -> Forest
parseForestRow = go empty 0 . fastUnpack where
  go : SortedSet (Int, Int) -> Int -> List Char -> Forest
  go trees width [] = MkForest width 1 trees
  go trees width (c::cs) = if c == '#'
    then go (insert (width, 0) trees) (width + 1) cs
    else go trees (width + 1) cs

parseForest : List String -> Forest
parseForest = concat . map parseForestRow 

-- note: if down /= 1, we teleport...
record Trajectory where
  constructor MkTrajectory
  right, down : Int

hasTree : Forest -> (Int, Int) -> Bool
hasTree f (x, y) = contains (x `mod` f.width, y) f.trees

path : (Int, Int) -> Trajectory -> Forest -> List (Int, Int)
path (x, y) t f = if y >= f.height
  then [(x, y)]
  else (x, y)::(path (x + t.right, y + t.down) t f)

getLines : IO (List String)
getLines = do
  eof <- fEOF stdin
  if eof
    then pure []
    else (::) <$> getLine <*> getLines

treesOnPath' : (Int, Int) -> Trajectory -> Forest -> Nat
treesOnPath' init t f = length $ L.filter (hasTree f) $ path init t f

treesOnPath : Trajectory -> Forest -> Nat
treesOnPath = treesOnPath' (0, 0)

main : IO ()
main = do
  forest <- parseForest <$> getLines
  printLn $ treesOnPath (MkTrajectory 3 1) forest
  let slopes = [ MkTrajectory 1 1
               , MkTrajectory 3 1
               , MkTrajectory 5 1
               , MkTrajectory 7 1
               , MkTrajectory 1 2
               ]
  printLn $ product $ map (`treesOnPath` forest) slopes
