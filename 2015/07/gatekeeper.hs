{-# LANGUAGE OverloadedStrings #-}

import Data.Word (Word16)
import Data.Bits
import Data.Maybe (maybe)
import Text.Read (readMaybe)
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict (State, gets, modify, evalState)

data Wire
  = Const Word16
  | Named T.Text
  | And Wire Wire
  | Or Wire Wire
  | LShift Wire Int
  | RShift Wire Int
  | Not Wire
  deriving (Eq, Show)

type Circuit = M.Map T.Text Wire
type CachingCircuit = M.Map T.Text (Either Wire Word16)

output :: Circuit -> Wire -> Word16
output _ (Const w) = w
output c (Named lbl) = maybe 0 (output c) (c M.!? lbl)
output c (And w1 w2) = output c w1 .&. output c w2
output c (Or w1 w2) = output c w1 .|. output c w2
output c (LShift w n) = shiftL (output c w) n
output c (RShift w n) = shiftR (output c w) n
output c (Not w) = complement (output c w)

output' :: Wire -> State CachingCircuit Word16
output' (Const w) = pure w
output' (Named lbl) = do
  cached <- gets (M.findWithDefault (Right 0) lbl)
  case cached of
    Right n -> pure n
    Left wire -> do
      val <- output' wire
      modify (M.insert lbl (Right val))
      pure val
output' (And w1 w2) = (.&.) <$> output' w1 <*> output' w2
output' (Or w1 w2) = (.|.) <$> output' w1 <*> output' w2
output' (LShift w n) = shiftL <$> output' w <*> pure n
output' (RShift w n) = shiftR <$> output' w <*> pure n
output' (Not w) = complement <$> output' w

{- note that the format we parse is limited compared to what we
 -   can represent... maybe this will pay off in part 2?
 - eta: no, it paid off in part 1, because the problem writer
 -   mispecified the input: my input had some lines like "1 AND foo -> bar"
 -}
parseDef :: T.Text -> Maybe (T.Text, Wire)
parseDef line = case T.split isSpace line of
  [sing, "->", dst] -> Just (dst, parseSingleton sing)
  [lhs, "AND", rhs, "->", dst] ->
    Just (dst, And (parseSingleton lhs) (parseSingleton rhs))
  [lhs, "OR", rhs, "->", dst] ->
    Just (dst, Or (parseSingleton lhs) (parseSingleton rhs))
  [sing, "LSHIFT", n, "->", dst] -> do
    shift <- readMaybe $ T.unpack n
    Just (dst, LShift (parseSingleton sing) shift)
  [sing, "RSHIFT", n, "->", dst] -> do
    shift <- readMaybe $ T.unpack n
    Just (dst, RShift (parseSingleton sing) shift)
  ["NOT", sing, "->", dst] -> Just (dst, Not (parseSingleton sing))
  _ -> Nothing
  where
    parseSingleton singleton = case readMaybe (T.unpack singleton) of
      Just n -> Const n
      Nothing -> Named singleton

main :: IO ()
main = do
  Just input <- traverse parseDef . T.lines <$> TIO.getContents
  let circuit = M.fromList input
      cachingCircuit = M.map Left circuit
      -- values = evalState (traverse output' $ M.elems circuit) cachingCircuit
  -- print values
  let aValue = evalState (output' $ circuit M.! "a") cachingCircuit
      circuit' = M.insert "b" (Const aValue) circuit
      cachingCircuit' = M.map Left circuit'
      aValue' = evalState (output' $ circuit M.! "a") cachingCircuit'
  print aValue
  print aValue'
