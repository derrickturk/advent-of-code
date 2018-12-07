{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module Workers (
    WorkerState(..)
  , workTime
  , duration
) where

import Control.Monad.State.Strict
import qualified Data.Set as S
import Data.Char (ord)
import Data.List (partition)
import Kahn

data WorkerState = Working Node Int
                 | Idle
                 deriving (Show, Eq)

workTime :: Int -> Node -> Int
workTime offset (Node c) = offset + ord c - ord 'A' + 1

data WorkingState = WorkingState { wsInitials :: S.Set Node
                                 , wsGraph :: Graph
                                 , wsWorkers :: [WorkerState]
                                 , wsTimeStep :: Int
                                 , wsOffset :: Int
                                 } deriving Show

initialWorking :: Graph -> Int -> Int -> WorkingState
initialWorking g n o = WorkingState (initials g) g (replicate n Idle) 0 o

duration :: Graph -> Int -> Int -> Int
duration g n o = evalState multiwork (initialWorking g n o)

assignWorkers :: MonadState WorkingState m => m ()
assignWorkers = do
  wks' <- gets wsWorkers >>= mapM assign
  modify (\ws -> ws { wsWorkers = wks' })
  where
    assign Idle = do
      s <- gets wsInitials
      if S.null s
        then pure Idle
        else do
          let (n, s') = S.deleteFindMin s
          modify (\ws -> ws { wsInitials = s' })
          o <- gets wsOffset
          pure $ Working n (workTime o n)
    assign w = pure w

doTimeStep :: MonadState WorkingState m => m ()
doTimeStep = do
  wks' <- gets wsWorkers >>= mapM doWork
  modify $ \ws -> ws { wsWorkers = wks', wsTimeStep = wsTimeStep ws + 1 }
  where
    doWork Idle = pure Idle
    doWork (Working n 1) = do
      finishNode n
      pure Idle
    doWork (Working n t) = pure $ Working n (t - 1)

finishNode :: MonadState WorkingState m => Node -> m ()
finishNode n = do
  (conn, notConn) <- partition ((== n) . from) <$> gets wsGraph
  mapM addInitial $
    filter (\m -> not $ any ((== m) . to) notConn) $ to <$> conn
  modify $ \ws -> ws { wsGraph = notConn }
  where
    addInitial m = modify $
      \ws -> ws { wsInitials = S.insert m (wsInitials ws) }

multiwork :: MonadState WorkingState m => m Int
multiwork = do
  assignWorkers
  done <- gets $ all (== Idle) . wsWorkers
  if done
    then gets wsTimeStep
    else do
      doTimeStep
      multiwork
