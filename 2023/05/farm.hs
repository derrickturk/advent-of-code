{-# LANGUAGE OverloadedRecordDot, OverloadedStrings #-}

import Prelude hiding (lookup)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Text (unpack)

import FemtoParsec

newtype FarmMap = FarmMap { unFarmMap :: M.Map Int (Int, Int) }
  deriving Show

lookup :: FarmMap -> Int -> Int
lookup (FarmMap m) k
  | Just (start, (vstart, len)) <- M.lookupLE k m
  , k < start + len
  = vstart + (k - start)
  | otherwise = k

type Almanac = [FarmMap]

chainLookup :: Almanac -> Int -> Int
chainLookup a k = foldl' (flip lookup) k a

farmMap :: Parser (String, String, FarmMap)
farmMap = do
  from <- letters
  _ <- "-to-"
  to <- letters
  _ <- lexeme " map:"
  mappings <- some $
    ((,,) <$> lexeme unsignedIntNum
          <*> lexeme unsignedIntNum
          <*> lexeme' unsignedIntNum)
  let tbl = M.fromList
        [(start, (vstart, len)) | (vstart, start, len) <- mappings]
  return (unpack from, unpack to, FarmMap tbl)

almanac :: Parser ([Int], [(String, String)], Almanac)
almanac = do
  _ <- "seeds: "
  inits <- lexeme $ sepBy " " unsignedIntNum
  maps <- some farmMap
  let names = (\(from, to, _) -> (from, to)) <$> maps
      maps' = (\(_, _, m) -> m) <$> maps
  return (inits, names, maps')

main :: IO ()
main = do
  Just (inits, _, a) <- parseStdin almanac
  print $ minimum $ chainLookup a <$> inits
