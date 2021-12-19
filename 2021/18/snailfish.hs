{-# LANGUAGE OverloadedStrings, TypeApplications #-}

import Data.List (foldl1')
import Prelude hiding (Left, Right)

import FemtoParsec
import Combo

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

focused :: Loc -> Snailfish
focused (Loc (s, _)) = s

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

root :: Loc -> Loc
root l = case parent l of
  Just l' -> root l'
  Nothing -> l

leftSibling :: Loc -> Maybe Loc
leftSibling (Loc (_, Top)) = Nothing
leftSibling (Loc (_, Left _ _)) = Nothing
leftSibling (Loc (s, Right path left)) = Just $ Loc (left, Left path s)

rightSibling :: Loc -> Maybe Loc
rightSibling (Loc (_, Top)) = Nothing
rightSibling (Loc (_, Right _ _)) = Nothing
rightSibling (Loc (s, Left path right)) = Just $ Loc (right, Right path s)

leftChild :: Loc -> Maybe Loc
leftChild (Loc (Val _, _)) = Nothing
leftChild (Loc (Pair l r, path)) = Just $ Loc (l, Left path r)

rightChild :: Loc -> Maybe Loc
rightChild (Loc (Val _, _)) = Nothing
rightChild (Loc (Pair l r, path)) = Just $ Loc (r, Right path l)

zipper :: Snailfish -> Loc
zipper s = Loc (s, Top)

leftNum :: Loc -> Maybe Loc
leftNum (Loc (_, Top)) = Nothing
-- TODO: cleanup
leftNum l = (leftSibling l >>= rightmost' p) <|> (parent l >>= leftNum) where
  p (Loc (Val _, _)) = True
  p _ = False

rightNum :: Loc -> Maybe Loc
rightNum (Loc (_, Top)) = Nothing
-- TODO: cleanup
rightNum l = (rightSibling l >>= leftmost' p) <|> (parent l >>= rightNum) where
  p (Loc (Val _, _)) = True
  p _ = False

leftmost :: (Loc -> Bool) -> Snailfish -> Maybe Loc
leftmost predicate s = leftmost' predicate (zipper s)

leftmost' :: (Loc -> Bool) -> Loc -> Maybe Loc
leftmost' predicate loc =  goLeft predicate loc
                       <|> rightHere predicate loc
                       <|> goRight predicate loc
  where
    goLeft p l = leftChild l >>= leftmost' p
    rightHere p l = if p l then Just l else Nothing
    goRight p l = rightChild l >>= leftmost' p

rightmost :: (Loc -> Bool) -> Snailfish -> Maybe Loc
rightmost predicate s = rightmost' predicate (zipper s)

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

leftmost10 :: Snailfish -> Maybe Loc
leftmost10 = leftmost p where
  p (Loc (Val n, _)) = n >= 10
  p _ = False

add :: Snailfish -> Snailfish -> Snailfish
add lhs rhs = reduce $ Pair lhs rhs

reduce1 :: Snailfish -> Maybe Snailfish
reduce1 s = explodeLeftmost4 s <|> splitLeftmost10 s

reduce :: Snailfish -> Snailfish
reduce s = case reduce1 s of
  Just s' -> reduce s'
  _ -> s

reduction :: Snailfish -> [Snailfish]
reduction s = s:case reduce1 s of
  Just s' -> reduction s'
  Nothing -> []

explodeLeftmost4 :: Snailfish -> Maybe Snailfish
explodeLeftmost4 s = do
  tgt@(Loc (Pair (Val x) (Val y), _)) <- leftmost4 s
  tgt' <- case leftNum tgt of
    Just n -> leftmost4 $ focused $ root $ modify n (addVal x)
    _ -> pure tgt
  tgt'' <- case rightNum tgt' of
    Just n -> leftmost4 $ focused $ root $ modify n (addVal y)
    _ -> pure tgt'
  let tgt''' = alter tgt'' (Val 0)
  pure $ focused $ root tgt'''
  where
    addVal x (Val y) = Val $ x + y
    addVal _ other = other

splitLeftmost10 :: Snailfish -> Maybe Snailfish
splitLeftmost10 s = do
  tgt@(Loc (Val x, _)) <- leftmost10 s
  let tgt' = alter tgt (Pair (Val $ halfDown x) (Val $ halfUp x))
  pure $ focused $ root tgt'
  where
    halfDown n = n `div` 2
    halfUp n = n `div` 2 + n `mod` 2

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

render :: Snailfish -> String
render (Val n) = show n
render (Pair lhs rhs) = "[" <> render lhs <> "," <> render rhs <> "]"

magnitude :: Snailfish -> Int
magnitude (Val n) = n
magnitude (Pair lhs rhs) = 3 * magnitude lhs + 2 * magnitude rhs

main :: IO ()
main = do
  Just fishes <- parseStdin (some $ lexeme snailfish)
  let total = foldl1' add fishes
  print $ magnitude total
  let pairs = do
        ([x, y], _) <- choose fishes 2
        [add x y, add y x]
  print $ maximum $ magnitude <$> pairs
