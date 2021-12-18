{-# LANGUAGE OverloadedStrings, TypeApplications #-}

import Data.Maybe (fromJust)

import FemtoParsec

data SnailfishElem
  = Val Int
  | Nest Snailfish
  deriving Show

data Snailfish
  = Pair SnailfishElem SnailfishElem
  deriving Show

data Step
  = L
  | R
  deriving Show

type Cursor = [Step]
type Zipper = (Snailfish, Cursor)

{-

readC :: Cursor -> Snailfish -> Maybe SnailfishElem
readC [L] (Pair lhs _) = Just lhs
readC [R] (Pair _ rhs) = Just rhs
readC (L:rest) (Pair (Nest lhs) _) = readC rest lhs
readC (R:rest) (Pair _ (Nest rhs)) = readC rest rhs
readC _ _ = Nothing

modifyC :: Cursor
        -> (SnailfishElem -> SnailfishElem)
        -> Snailfish
        -> Maybe Snailfish
modifyC [L] f (Pair lhs rhs) = Just $ Pair (f lhs) rhs
modifyC [R] f (Pair lhs rhs) = Just $ Pair lhs (f rhs)
modifyC (L:rest) f (Pair (Nest lhs) rhs) =
  Pair <$> (Nest <$> modifyC rest f lhs) <*> pure rhs
modifyC (R:rest) f (Pair lhs (Nest rhs)) =
  Pair <$> pure lhs <*> (Nest <$> modifyC rest f rhs)
modifyC _ _ _ = Nothing

writeC :: Cursor -> SnailfishElem -> Snailfish -> Maybe Snailfish
writeC cur e s = modifyC cur (const e) s

stepLeft :: Cursor -> Maybe Cursor
stepLeft [] = Nothing
stepLeft [R] = Just [L]
stepLeft [L] = Nothing
stepLeft (L:rest) = (L:) <$> stepLeft rest
stepLeft (R:rest) = (R:) <$> stepLeft rest <|> Just (L:(R <$ rest))

stepRight :: Cursor -> Maybe Cursor
stepRight [] = Nothing
stepRight [L] = Just [R]
stepRight [R] = Nothing
stepRight (R:rest) = (R:) <$> stepRight rest
stepRight (L:rest) = (L:) <$> stepRight rest <|> Just (R:(L <$ rest))

lefties :: Cursor -> [Cursor]
lefties c = case stepLeft c of
  Just c' -> c':lefties c'
  Nothing -> []

righties :: Cursor -> [Cursor]
righties c = case stepRight c of
  Just c' -> c':righties c'
  Nothing -> []
-}

{-
add :: Snailfish -> Snailfish -> Snailfish
add lhs rhs = reduce $ Pair (Nest lhs) (Nest rhs)

reduce1 :: Snailfish -> Maybe Snailfish
reduce1 s = explodeLeftmost4 s <|> splitLeftmost10 s

reduce :: Snailfish -> Snailfish
reduce s = case reduce1 s of
  Just s' -> reduce s'
  _ -> s

explodeLeftmost4 :: Snailfish -> Maybe Snailfish
explodeLeftmost4 pair@(Pair lhs rhs) = do
  (x, y, path) <- go 1 [L] lhs <|> go 1 [R] rhs
  let path' = reverse path
  fixup pair x y path
  where
    go n p (Nest (Pair (Val x) (Val y)))
      | n >= 4 = Just (x, y, p)
      | otherwise = Nothing
    go _ _ (Val _) = Nothing
    go n p (Nest (Pair lhs rhs)) =
      go (n + 1) (L:p) lhs <|> go (n + 1) (R:p) rhs

    fixup pair x y p = pushLeft x p $ pushRight y p $ zero pair p

    zero () [] = Just e
    update (L:p) (Nest (Pair lhs rhs)) = Nest ( -- who fucking knows

splitLeftmost10 :: Snailfish -> Maybe Snailfish
splitLeftmost10 (Pair lhs rhs) =
  case splitLeftmost10' lhs of
    Just lhs' -> Just $ Pair lhs' rhs
    _ -> case splitLeftmost10' rhs of
      Just rhs' -> Just $ Pair lhs rhs'
  where
    splitLeftmost10' (Val n)
      | n >= 10 = Just $ Nest (Pair (Val $ halfDown n) (Val $ halfUp n))
      | otherwise = Nothing
    splitLeftmost10' (Nest p) = Nest <$> splitLeftmost10 p

    halfDown :: Int -> Int
    halfDown n = floor $ (fromIntegral @_ @Double n) / 2.0

    halfUp :: Int -> Int
    halfUp n = ceiling $ (fromIntegral @_ @Double n) / 2.0

explode :: Snailfish -> (Int, Int, Snailfish)
explode (Pair (Val x) (Val y)) = (x, y, Pair (Val 0) (Val 0))
explode p = (0, 0, p)
-}

snailfish :: Parser Snailfish
snailfish = do
  _ <- "["
  lhs <- snailElem
  _ <- ","
  rhs <- snailElem
  _ <- "]"
  pure $ Pair lhs rhs
  where
    snailElem =  Val <$> unsignedIntNum
             <|> Nest <$> snailfish

main :: IO ()
main = do
  Just fishes <- parseStdin (some $ lexeme snailfish)
  print fishes
