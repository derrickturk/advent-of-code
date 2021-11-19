{-# LANGUAGE OverloadedStrings #-}

-- no funny business, this isn't a knapsack

import Data.Char (isSpace)
import Data.Ord (comparing)
import Data.List (maximumBy)
import Data.Foldable (foldMap')
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Ingredient = Ingredient { capacity :: Integer
                             , durability :: Integer
                             , flavor :: Integer
                             , texture :: Integer
                             , calories :: Integer
                             } deriving (Eq, Show)

instance Semigroup Ingredient where 
  Ingredient c1 d1 f1 t1 cal1 <> Ingredient c2 d2 f2 t2 cal2 =
    Ingredient (c1 + c2) (d1 + d2) (f1 + f2) (t1 + t2) (cal1 + cal2)

instance Monoid Ingredient where
  mempty = Ingredient 0 0 0 0 0

(*!) :: Ingredient -> Integer -> Ingredient
Ingredient c d f t cal *! n =
  Ingredient (n * c) (n * d) (n * f) (n * t) (n * cal)

type Recipe = [(Ingredient, Integer)]

bake :: Recipe -> Ingredient
bake = foldMap' id . fmap (uncurry (*!))

score :: Ingredient -> Integer
score (Ingredient c d f t _)
  | c < 0 || d < 0 || f < 0 || t < 0 = 0
  | otherwise = c * d * f * t

best :: Integer -> [Ingredient] -> Recipe
best n ings = maximumBy (comparing (score . bake)) $ allRecipes n ings

allRecipes :: Integer -> [Ingredient] -> [Recipe]
allRecipes _ [] = []
allRecipes n [ing] = [[(ing, n)]]
allRecipes n (ing:rest) = do
  k <- [0..n]
  tails <- allRecipes (n - k) rest
  pure $ (ing, k):tails

bestWithCalories :: Integer -> Integer -> [Ingredient] -> Recipe
bestWithCalories n cals ings = maximumBy (comparing (score . bake)) $
  filter ((== cals) . calories . bake) $ allRecipes n ings

parseIngredient :: T.Text -> Maybe (T.Text, Ingredient)
parseIngredient line = case T.split isSpace line of
  [ name
    , "capacity"
    , cap
    , "durability"
    , dur
    , "flavor"
    , flv
    , "texture"
    , txt
    , "calories"
    , cal
    ] -> do
      let name' = T.init name
      cap' <- readMaybe $ T.unpack $ T.init cap
      dur' <- readMaybe $ T.unpack $ T.init dur
      flv' <- readMaybe $ T.unpack $ T.init flv
      txt' <- readMaybe $ T.unpack $ T.init txt
      cal' <- readMaybe $ T.unpack cal
      pure (name', Ingredient cap' dur' flv' txt' cal')
  _ -> Nothing

main :: IO ()
main = do
  Just ings <- traverse parseIngredient . T.lines <$> TIO.getContents
  let ings' = snd <$> ings
  print $ score $ bake $ best 100 ings'
  print $ score $ bake $ bestWithCalories 100 500 ings'
