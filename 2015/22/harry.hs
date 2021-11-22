{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLabels #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)
import Data.Char (isSpace)
import Control.Monad (foldM)

data Player
  = Player { playerHP :: Int
           , armor :: Int
           , mana :: Int
           } deriving (Eq, Show)

newPlayer :: Player
newPlayer = Player { playerHP = 50
                   , armor = 0
                   , mana = 500
                   }

data Boss
  = Boss { bossHP :: Int
         , damage :: Int
         } deriving (Eq, Show)

parseBoss :: T.Text -> Maybe Boss
parseBoss input = case T.split isSpace input of
  ["Hit", "Points:", hp, "Damage:", dmg, _] ->
    Boss <$> readMaybe (T.unpack hp) <*> readMaybe (T.unpack dmg)
  _ -> Nothing

data GameState
  = GameState { player :: Player
              , boss :: Boss
              , shieldTurns :: Int
              , poisonTurns :: Int
              , rechargeTurns :: Int
              } deriving (Eq, Show)

newGame :: Boss -> GameState
newGame gameBoss = GameState { player = newPlayer
                             , boss = gameBoss
                             , shieldTurns = 0
                             , poisonTurns = 0
                             , rechargeTurns = 0
                             }

type PlayerAction = GameState -> GameState

castMagicMissile :: PlayerAction
castMagicMissile game =
  game { player = (player game) { mana = (mana $ player game) - 53 }
       , boss = (boss game) { bossHP = (bossHP $ boss game) - 4 }
       }

castDrain :: PlayerAction
castDrain game =
  game { player = (player game) { mana = (mana $ player game) - 73
                                , playerHP = (playerHP $ player game) + 2
                                }
       , boss = (boss game) { bossHP = (bossHP $ boss game) - 2 }
       }

castShield :: PlayerAction
castShield game =
  game { player = (player game) { mana = (mana $ player game) - 113
                                , armor = (armor $ player game) + 7
                                }
       , shieldTurns = 6
       }

castPoison :: PlayerAction
castPoison game =
  game { player = (player game) { mana = (mana $ player game) - 173 }
       , poisonTurns = 6
       }

castRecharge :: PlayerAction
castRecharge game =
  game { player = (player game) { mana = (mana $ player game) - 229 }
       , rechargeTurns = 5
       }

applyPoison :: GameState -> GameState
applyPoison game = case poisonTurns game of
  0 -> game
  n -> game { boss = (boss game) { bossHP = (bossHP $ boss game) - 3 }
            , poisonTurns = n - 1
            }

applyShield :: GameState -> GameState
applyShield game = case shieldTurns game of
  0 -> game
  1 -> game { player = (player game) { armor = (armor $ player game) - 7 }
            , shieldTurns = 0
            }
  n -> game { shieldTurns = n - 1 }

applyRecharge :: GameState -> GameState
applyRecharge game = case rechargeTurns game of
  0 -> game
  n -> game { player = (player game) { mana = (mana $ player game) + 101 }
            , rechargeTurns = n - 1
            }

sanctionedActions :: GameState -> [(Int, PlayerAction)]
sanctionedActions game = concat $
  [ if mana (player game) >= 53 then [(53, castMagicMissile)] else []
  , if mana (player game) >= 73 then [(73, castDrain)] else []
  , if mana (player game) >= 113 && shieldTurns game == 0
      then [(113, castShield)]
      else []
  , if mana (player game) >= 173 && poisonTurns game == 0
      then [(173, castPoison)]
      else []
  , if mana (player game) >= 229 && rechargeTurns game == 0
      then [(229, castRecharge)]
      else []
  ]

runEvents :: GameState -> GameState
runEvents = applyShield . applyPoison . applyRecharge

bossAttack :: GameState -> GameState
bossAttack game
  | bossHP (boss game) <= 0 = game
  | otherwise =
      let dmgAdj = max 1 $ damage (boss game) - armor (player game)
          newHP = (playerHP $ player game) - dmgAdj
       in game { player = (player game) { playerHP = newHP } }

playRound :: PlayerAction -> GameState -> GameState
playRound = (runEvents .) . (bossAttack .) . (runEvents .)

data GameOver
  = Win
  | Loss
  deriving (Eq, Ord, Show)

playRound' :: PlayerAction -> GameState -> Either GameOver GameState
playRound' action game = check $ playRound action game where
  check game'
    | playerHP (player game') <= 0 = Left Loss
    | bossHP (boss game') <= 0 = Left Win
    | null (sanctionedActions game') = Left Loss
    | otherwise = Right game'

example1Game :: GameState
example1Game = GameState { player = Player { playerHP = 10
                                           , mana = 250
                                           , armor = 0
                                           }
                         , boss = Boss { bossHP = 13, damage = 8 }
                         , shieldTurns = 0
                         , poisonTurns = 0
                         , rechargeTurns = 0
                         }

example1Actions :: [PlayerAction]
example1Actions = [ castPoison
                  , castMagicMissile
                  ]

example2Game :: GameState
example2Game = GameState { player = Player { playerHP = 10
                                           , mana = 250
                                           , armor = 0
                                           }
                         , boss = Boss { bossHP = 14, damage = 8 }
                         , shieldTurns = 0
                         , poisonTurns = 0
                         , rechargeTurns = 0
                         }

example2Actions :: [PlayerAction]
example2Actions = [ castRecharge
                  , castShield
                  , castDrain
                  , castPoison
                  , castMagicMissile
                  ]

main :: IO ()
main = do
  Just gameBoss <- parseBoss <$> TIO.getContents
  let game = newGame gameBoss
      ex1 = foldl (flip playRound) example1Game example1Actions
      ex2 = foldl (flip playRound) example2Game example2Actions
      ex1' = foldM (flip playRound') example1Game example1Actions
      ex2' = foldM (flip playRound') example2Game example2Actions
  print game
  print ex1
  print ex2
  print ex1'
  print ex2'
