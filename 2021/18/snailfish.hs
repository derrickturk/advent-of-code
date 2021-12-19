{-# LANGUAGE OverloadedStrings, TypeApplications #-}

import Prelude hiding (Left, Right)

import FemtoParsec

import Debug.Trace

-- this is technically wrong but the other way made life miserable
data Snailfish
  = Val Int
  | Pair Snailfish Snailfish
  deriving Show

data Path
  = Top
  | Left Path Snailfish
  | Right Path Snailfish
  deriving Show

newtype Loc = Loc { unLoc :: (Snailfish, Path) }
  deriving Show

depth :: Path -> Int
depth Top = 0
depth (Left p _) = 1 + depth p
depth (Right p _) = 1 + depth p

alter :: Loc -> Snailfish -> Loc
alter (Loc (_, p)) s = Loc (s, p)

modify :: Loc -> (Snailfish -> Snailfish) -> Loc
modify (Loc (s, p)) f = Loc (f s, p)

parent :: Loc -> Maybe Loc
parent (Loc (_, Top)) = Nothing
parent (Loc (s, Left path right)) = Just $ Loc (Pair s right, path)
parent (Loc (s, Right path left)) = Just $ Loc (Pair left s, path)

leftSibling :: Loc -> Maybe Loc
leftSibling (Loc (_, Top)) = Nothing
leftSibling (Loc (_, Left _ _)) = Nothing
leftSibling (Loc (s, Right path left)) = Just $ Loc (left, Left path s)

rightSibling :: Loc -> Maybe Loc
rightSibling (Loc (_, Top)) = Nothing
rightSibling (Loc (_, Right _ _)) = Nothing
rightSibling (Loc (s, Left path right)) = Just $ Loc (right, Left path s)

leftChild :: Loc -> Maybe Loc
leftChild (Loc (Val _, _)) = Nothing
leftChild (Loc (Pair l r, path)) = Just $ Loc (l, Left path r)

rightChild :: Loc -> Maybe Loc
rightChild (Loc (Val _, _)) = Nothing
rightChild (Loc (Pair l r, path)) = Just $ Loc (r, Right path l)

root :: Snailfish -> Loc
root s = Loc (s, Top)

leftNum :: Loc -> Maybe Loc
leftNum (Loc (_, Top)) = Nothing
leftNum (Loc (s, Right path left@(Val _))) = Just $ Loc (left, Left path s)
leftNum l = parent l >>= go where
  go l' = (leftSibling l' >>= leftmost' p) <|> (parent l' >>= go)
  p (Loc (Val _, _)) = True
  p _ = False

rightNum :: Loc -> Maybe Loc
rightNum (Loc (_, Top)) = Nothing
rightNum (Loc (s, Left path right@(Val _))) = Just $ Loc (right, Right path s)
rightNum l = parent l >>= go where
  go l' = (rightSibling l' >>= rightmost' p) <|> (parent l' >>= go)
  p (Loc (Val _, _)) = True
  p _ = False

leftmost :: (Loc -> Bool) -> Snailfish -> Maybe Loc
leftmost predicate s = leftmost' predicate (root s)

leftmost' :: (Loc -> Bool) -> Loc -> Maybe Loc
leftmost' predicate loc =  goLeft predicate loc
                       <|> rightHere predicate loc
                       <|> goRight predicate loc
  where
    goLeft p l = leftChild l >>= leftmost' p
    rightHere p l = if p l then Just l else Nothing
    goRight p l = rightChild l >>= leftmost' p

rightmost :: (Loc -> Bool) -> Snailfish -> Maybe Loc
rightmost predicate s = rightmost' predicate (root s)

rightmost' :: (Loc -> Bool) -> Loc -> Maybe Loc
rightmost' predicate loc =  goRight predicate loc
                        <|> rightHere predicate loc
                        <|> goLeft predicate loc
  where
    goRight p l = rightChild l >>= rightmost' p
    rightHere p l = if p l then Just l else Nothing
    goLeft p l = leftChild l >>= rightmost' p

leftmost4 :: Snailfish -> Maybe Loc
leftmost4 = leftmost p where
  p (Loc (Pair _ _, path)) = depth path >= 4
  p _ = False

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
             <|> snailfish

main :: IO ()
main = do
  Just fishes <- parseStdin (some $ lexeme snailfish)
  print $ leftmost4 $ head fishes
  print $ (leftmost4 $ head fishes) >>= leftNum
  print $ (leftmost4 $ head fishes) >>= rightNum
