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

-- overlap between intervals, that-start <= this-start
overlapLE :: (Int, Int) -> (Int, Int) -> (Maybe (Int, Int), Maybe (Int, Int))
overlapLE (thisStart, thisLen) (thatStart, thatLen) =
  let beginsIn = thisStart < thatStart + thatLen
      endsIn = thisStart + thisLen < thatStart + thatLen
   in case (beginsIn, endsIn) of
        (True, True) -> (Just (thisStart, thisLen), Nothing)
        (False, False) -> (Nothing, Just (thisStart, thisLen))
        (True, False) ->
          let overlap = thatStart + thatLen - thisStart
           in (
                Just (thisStart, overlap)
              , Just (thatStart + thatLen, thisLen - overlap)
              )
        _ -> error "precondition violated"

-- overlap between intervals, that-start > this-start
overlapGT :: (Int, Int)
          -> (Int, Int)
          -> (Maybe (Int, Int), Maybe (Int, Int), Maybe (Int, Int))
overlapGT (thisStart, thisLen) (thatStart, thatLen) =
  let endsBefore = thisStart + thisLen < thatStart
      endsPast = thisStart + thisLen > thatStart + thatLen
   in case (endsBefore, endsPast) of
        (True, False) -> (Just (thisStart, thisLen), Nothing, Nothing)
        (False, False) ->
          let overlap = thisStart + thisLen - thatStart
           in ( Just (thisStart, thisLen - overlap)
              , Just (thatStart, overlap)
              , Nothing
              )
        (False, True) ->
          ( Just (thisStart, thatStart - thisStart)
          , Just (thatStart, thatLen)
          , Just (thatStart + thatLen, thisStart + thisLen - (thatStart + thatLen))
          )
        _ -> error "precondition violated"

lookups :: FarmMap -> Int -> Int -> [(Int, Int)]
lookups _ _ 0 = []
lookups fm@(FarmMap m) kstart klen =
  case M.lookupLE kstart m of
    Just (start, (vstart, len)) | kstart < start + len ->
      case overlapLE (kstart, klen) (start, len) of
        (Just (ostart, olen), Nothing) -> [(ostart + (vstart - start), olen)]
        (Just (ostart, olen), Just (nostart, nolen)) ->
          (ostart + (vstart - start), olen):lookups fm nostart nolen
        _ -> error "impossible case"
    _ ->
      case M.lookupGT kstart m of
        Just (start, (vstart, len)) ->
          case overlapGT (kstart, klen) (start, len) of
            (Just (lnostart, lnolen), Nothing, Nothing) -> [(lnostart, lnolen)]
            (Just (lnostart, lnolen), Just (ostart, olen), Nothing) ->
              [(lnostart, lnolen), (ostart + (vstart - start), olen)]
            (Just (lnostart, lnolen), Just (ostart, olen), Just (rnostart, rnolen)) ->
              (lnostart, lnolen):(ostart + (vstart - start), olen):lookups fm rnostart rnolen
            _ -> error "impossible case"
        Nothing -> [(kstart, klen)]

type Almanac = [FarmMap]

chainLookup :: Almanac -> Int -> Int
chainLookup a k = foldl' (flip lookup) k a

chainLookups :: Almanac -> (Int, Int) -> [(Int, Int)]
chainLookups [] (start, len) = [(start, len)]
chainLookups (m:ms) (start, len) = do
  (vstart, vlen) <- lookups m start len
  chainLookups ms (vstart, vlen)

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

pairs :: [Int] -> [(Int, Int)]
pairs (x:y:rest) = (x, y):pairs rest
pairs _ = []

main :: IO ()
main = do
  Just (inits, _, a) <- parseStdin almanac
  print $ minimum $ chainLookup a <$> inits
  let ranges = pairs inits
      outs = concatMap (chainLookups a) ranges
  print $ minimum $ fst <$> outs
