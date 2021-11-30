-- I'm not even messing with the official input format
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (isSpace)
import Text.Read (readMaybe)
import Control.Monad (guard)

data Node = Node { name :: T.Text
                 , pos :: (Int, Int)
                 , size :: Int
                 , used :: Int
                 , avail :: Int
                 , usedPercent :: Int
                 } deriving (Eq, Show)

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

main :: IO ()
main = do
  Just nodes <- traverse parseNode . T.lines <$> TIO.getContents
  print $ length $ goodPairs nodes
