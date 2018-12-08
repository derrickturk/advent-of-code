{-# LANGUAGE LambdaCase, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Text.Read (readMaybe)
import Control.Monad (replicateM, (<=<))
import Data.List (uncons)

data Node a = Node { children :: [Node a]
                   , metadata :: [a]
                   } deriving (Show, Eq, Functor, Foldable, Traversable)

newtype Parser a = Parser { runParser :: [Int] -> Maybe (a, [Int]) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \xs -> do
    (a, i) <- p xs
    pure (f a, i)

instance Applicative Parser where
  pure a = Parser $ \xs -> Just (a, xs)
  (Parser pf) <*> (Parser pa) = Parser $ \xs -> do
    (f, i) <- pf xs
    (a, i') <- pa i
    pure (f a, i')

instance Monad Parser where
  (Parser pa) >>= f = Parser $ \xs -> do
    (a, i) <- pa xs
    (b, i') <- runParser (f a) i
    pure (b, i')

end :: Parser ()
end = Parser $ \case
  [] -> Just ((), [])
  _ -> Nothing

next :: Parser Int
next = Parser uncons

parseNode :: Parser (Node Int)
parseNode = do
  nChildren <- next
  nMeta <- next
  Node <$> replicateM nChildren parseNode <*> replicateM nMeta next

only :: Parser a -> Parser a
only p = do
  r <- p
  end
  pure r

parseInput :: String -> Maybe (Node Int)
parseInput =
  fmap fst . runParser (only parseNode) <=< traverse readMaybe . words

value :: Node Int -> Int
value (Node [] m) = sum m
value (Node cs m) = sum (getChildValue cs <$> m) where 
  getChildValue cs i
    | i == 0 || i > length cs = 0
    | otherwise = value $ cs !! (i - 1)

main :: IO ()
main = do
  nums <- parseInput <$> getContents
  case nums of
    Just node -> do
      print $ sum node
      print $ value node
    Nothing -> putStrLn "invalid input"
