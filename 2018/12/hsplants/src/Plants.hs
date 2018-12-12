module Plants (
    Plants(..)
  , Rule(..)
  , compileRules
  , slicePlants
  , applyRules
  , iterateRules
  , prettyPlants
  , plantPots
) where

import qualified Data.Vector.Unboxed as V
import qualified Data.IntMap.Strict as M
import Data.Bits (shiftL, (.|.))
import Data.Bool (bool)

data Plants = Plants Int (V.Vector Bool)
  deriving Show

data Rule = Rule (V.Vector Bool) Bool
  deriving Show

type RuleSet = M.IntMap Bool

compileRules :: [Rule] -> RuleSet
compileRules = M.fromList . fmap compileRule

compileRule :: Rule -> (Int, Bool)
compileRule (Rule vec r) = (compilePlants vec, r)

compilePlants :: V.Vector Bool -> Int
compilePlants = V.ifoldl (\n i b -> n .|. shiftL (fromEnum b) i) 0

-- rules are 5 wide, so we need to make sure we have at least 4 elements before
--   the first plant and after the last plant
expandPlantsLeft :: Plants -> Plants
expandPlantsLeft (Plants i0 vec) = case V.findIndex id vec of
  Just i | i < 4 -> let needed = 4 - i
                    in Plants (i0 - needed) (V.replicate needed False <> vec)
  _ -> Plants i0 vec

expandPlantsRight :: Plants -> Plants
expandPlantsRight (Plants i0 vec) = case V.findIndex id (V.reverse vec) of
  Just i | i < 4 -> let needed = 4 - i
                    in Plants i0 (vec <> V.replicate needed False)
  _ -> Plants i0 vec

expandPlants :: Plants -> Plants
expandPlants = expandPlantsLeft . expandPlantsRight

slicePlants :: Int -> Int -> Plants -> V.Vector Bool
slicePlants i n (Plants i0 vec) = V.slice (i - i0) n vec

applyRules :: RuleSet -> Plants -> Plants
applyRules rs (Plants i0 vec) =
  Plants i0 $ V.generate (V.length vec) runPlant
  where
    runPlant i
      | i < 2 || i >= V.length vec - 2 = vec V.! i
      | otherwise = case rs M.!? compilePlants (V.slice (i - 2) 5 vec) of
          Just r -> r
          Nothing -> False -- the example leaves "no-plant" rules implicit

iterateRules :: RuleSet -> Plants -> [Plants]
iterateRules r = iterate (applyRules r . expandPlants)

prettyPlants :: Plants -> String
prettyPlants (Plants i0 vec) = V.toList (V.map (bool '.' '#') vec)
  ++ "\n" ++ replicate (-i0) ' ' ++ "^"

plantPots :: Plants -> [Int]
plantPots (Plants i0 vec) = V.ifoldr ixIfPlant [] vec where
  ixIfPlant _ False ixs = ixs
  ixIfPlant i True ixs = (i + i0):ixs
