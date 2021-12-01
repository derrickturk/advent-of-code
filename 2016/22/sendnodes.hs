-- I'm not even messing with the official input format
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (isSpace)
import Data.List (find, maximumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Text.Read (readMaybe)
import Control.Monad (guard)

data Node = Node { name :: T.Text
                 , pos :: (Int, Int)
                 , size :: Int
                 , used :: Int
                 , avail :: Int
                 , usedPercent :: Int
                 } deriving (Eq, Show)

data MazeCell
  = Empty
  | Wall
  | Goal
  | Boring
  deriving Show

parseNode :: T.Text -> Maybe Node
parseNode line = case T.split isSpace line of
  [n, x, y, s, u, a, p] ->
    Node n <$> ((,) <$> readMaybe (T.unpack x) <*> readMaybe (T.unpack y))
           <*> readMaybe (T.unpack s)
           <*> readMaybe (T.unpack u)
           <*> readMaybe (T.unpack a)
           <*> readMaybe (T.unpack p)
  _ -> Nothing

goodPairs :: [Node] -> [(Node, Node)]
goodPairs nodes = do
  a <- nodes
  b <- nodes
  guard $ a /= b && used a /= 0 && used a <= avail b
  pure (a, b)

dumpMaze :: [Node] -> [[MazeCell]]
dumpMaze nodes =
  let highestX = fst $ pos $ maximumBy (comparing (fst . pos)) nodes
      highestY = snd $ pos $ maximumBy (comparing (snd . pos)) nodes
      cell node
        | used node == 0 = Empty
        | size node > 100 = Wall
        | pos node == (highestX, 0) = Goal
        | otherwise = Boring
   in [[cell $ fromJust $ find ((== (x, y)) . pos) nodes | x <- [0..highestX]] | y <- [0..highestY]]

cellChar :: MazeCell -> Char
cellChar Boring = '.'
cellChar Empty = '_'
cellChar Goal = 'G'
cellChar Wall = '#'

main :: IO ()
main = do
  Just nodes <- traverse parseNode . T.lines <$> TIO.getContents
  print $ length $ goodPairs nodes
  mapM_ (putStrLn . fmap cellChar) $ dumpMaze nodes
