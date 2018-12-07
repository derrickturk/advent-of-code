{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module Kahn (
    Node(..)
  , Edge(..)
  , Graph
  , topoSort
) where

import Control.Monad.State.Strict
import qualified Data.Set as S
import Data.List (foldl', partition)

newtype Node = Node { nodeLabel :: Char }
  deriving (Show, Eq, Ord)

data Edge = Edge { from :: Node
                 , to :: Node
                 } deriving (Show, Eq, Ord)

type Graph = [Edge]

-- Kahn's algorithm
topoSort :: Graph -> Maybe [Node]
topoSort = evalState kahn . initialKahn

data KahnState = KahnState { ksInitials :: S.Set Node
                           , ksGraph :: Graph
                           , ksSorted :: [Node]
                           } deriving Show

initialKahn :: Graph -> KahnState
initialKahn g = KahnState (initials g) g []

-- initials are from but never to
initials :: Graph -> S.Set Node
initials = uncurry S.difference . foldl' update (S.empty, S.empty) where
  update (froms, tos) (Edge from to) = (S.insert from froms, S.insert to tos)

kahn :: MonadState KahnState m => m (Maybe [Node])
kahn = do
  done <- gets $ S.null . ksInitials
  if done
    then gets ksGraph >>= \case
      [] -> Just . reverse <$> gets ksSorted
      _ -> pure Nothing
    else do
      n <- shiftInitial
      scanGraph n 
      kahn
  where
    shiftInitial = do
      (n, s') <- gets $ S.deleteFindMin . ksInitials
      modify $ \ks -> ks { ksInitials = s', ksSorted = n : ksSorted ks }
      pure n
    scanGraph n = do
      (conn, notConn) <- partition ((== n) . from) <$> gets ksGraph
      mapM addInitial $
        filter (\m -> not $ any ((== m) . to) notConn) $ to <$> conn
      modify $ \ks -> ks { ksGraph = notConn }
    addInitial m = modify $
      \ks -> ks { ksInitials = S.insert m (ksInitials ks) }
