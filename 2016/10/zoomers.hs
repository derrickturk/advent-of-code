{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map.Strict as M
import Data.List (find, foldl')
import Control.Monad.State (State, gets, modify, evalState)

import FemtoParsec

data Target
  = Bot Int
  | Output Int
  deriving Show

data Instruction
  = ValueGoes Int Int
  | BotGives Int Target Target
  deriving Show

target :: Parser Target
target =  Bot <$> ("bot " *> unsignedIntNum)
      <|> Output <$> ("output " *> unsignedIntNum)

instruction :: Parser Instruction
instruction =  ValueGoes <$> ("value " *> unsignedIntNum)
                         <*> (" goes to bot " *> unsignedIntNum)
           <|> BotGives <$> ("bot " *> unsignedIntNum)
                        <*> (" gives low to " *> target)
                        <*> (" and high to " *> target)

data BotSource
  = Value Int
  | BotLow Int
  | BotHigh Int
  deriving Show

botSources :: [Instruction] -> M.Map Int [BotSource]
botSources = foldl' f M.empty where
  f m (ValueGoes val bot) = M.insertWith (<>) bot [Value val] m 
  f m (BotGives _ (Output _) (Output _)) = m
  f m (BotGives srcBot (Bot loBot) (Output _)) =
    M.insertWith (<>) loBot [BotLow srcBot] m
  f m (BotGives srcBot (Output _) (Bot hiBot)) =
    M.insertWith (<>) hiBot [BotHigh srcBot] m
  f m (BotGives srcBot (Bot loBot) (Bot hiBot)) =
    M.insertWith (<>) loBot [BotLow srcBot] $
      M.insertWith (<>) hiBot [BotHigh srcBot] m

{- too slow
evalBotSources :: M.Map Int [BotSource] -> [BotSource] -> [Int]
evalBotSources m = map evalBotSource where
  evalBotSource (Value n) = n
  evalBotSource (BotLow bot) = minimum $ evalBotSources m (m M.! bot)
  evalBotSource (BotHigh bot) = maximum $ evalBotSources m (m M.! bot)
-}

evalBotSources' :: Int -> State (M.Map Int [Either BotSource Int]) [Int]
evalBotSources' bot = do
  srcs <- gets (M.! bot)
  vals <- traverse evalBotSource' srcs
  modify (M.insert bot $ Right <$> vals)
  pure vals
  where
    evalBotSource' (Right n) = pure n
    evalBotSource' (Left (Value n)) = pure n
    evalBotSource' (Left (BotLow i)) = minimum <$> evalBotSources' i
    evalBotSource' (Left (BotHigh i)) = maximum <$> evalBotSources' i

data OutputSource
  = OutputBotLow Int
  | OutputBotHigh Int
  deriving Show

outputSources :: [Instruction] -> M.Map Int [OutputSource]
outputSources = foldl' f M.empty where
  f m (ValueGoes _ _) = m
  f m (BotGives _ (Bot _) (Bot _)) = m
  f m (BotGives srcBot (Output loOut) (Bot _)) =
    M.insertWith (<>) loOut [OutputBotLow srcBot] m
  f m (BotGives srcBot (Bot _) (Output hiOut)) =
    M.insertWith (<>) hiOut [OutputBotHigh srcBot] m
  f m (BotGives srcBot (Output loOut) (Output hiOut)) =
    M.insertWith (<>) loOut [OutputBotLow srcBot] $
      M.insertWith (<>) hiOut [OutputBotHigh srcBot] m

evalOutputs :: M.Map Int [OutputSource] -> M.Map Int [Int] -> M.Map Int [Int]
evalOutputs osrcs bots = M.map (map evalO) osrcs where
  evalO (OutputBotLow bot) = minimum $ bots M.! bot
  evalO (OutputBotHigh bot) = maximum $ bots M.! bot

main :: IO ()
main = do
  Just instrs <- parseStdin $ many $ lexeme instruction
  let srcs = botSources instrs
      srcs' = M.map (map Left) srcs
      srcs'' = zip (M.keys srcs') $
        evalState (traverse evalBotSources' $ M.keys srcs') srcs'
      osrcs = outputSources instrs
      outputs = evalOutputs osrcs $ M.fromList srcs''
  print $ find (\(_, xs) -> xs == [17, 61] || xs == [61, 17]) srcs''
  print $ product $ [x | i <- [0..2], x <- outputs M.! i]
