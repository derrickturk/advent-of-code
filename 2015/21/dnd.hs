import Control.Monad (guard)
import Data.Maybe (maybeToList)

data Item
  = Item { itemName :: String
         , itemCost :: Int
         , itemDamage :: Int
         , itemArmor :: Int
         } deriving (Eq, Show)

weapons :: [Item]
weapons =
  [ Item "Dagger" 8 4 0
  , Item "Shortsword" 10 5 0
  , Item "Warhammer" 25 6 0
  , Item "Longsword" 40 7 0
  , Item "Greataxe" 74 8 0
  ]

armor :: [Item]
armor =
  [ Item "Leather" 13 0 1
  , Item "Chainmail" 31 0 2
  , Item "Splintmail" 53 0 3
  , Item "Bandedmail" 75 0 4
  , Item "Platemail" 102 0 5
  ]

rings :: [Item]
rings =
  [ Item "Damage +1" 25 1 0
  , Item "Damage +2" 50 2 0
  , Item "Damage +3" 100 3 0
  , Item "Defense +1" 20 0 1
  , Item "Defense +2" 40 0 2
  , Item "Defense +3" 80 0 3
  ]

data Player
  = Player { playerHP :: Int
           , playerDamage :: Int
           , playerArmor :: Int
           } deriving (Eq, Show)

playerCharacter :: [Item] -> Player
playerCharacter items =
  Player 100 (sum $ itemDamage <$> items) (sum $ itemArmor <$> items)

hitsToKill :: Player -> Player -> Int
hitsToKill p1 p2 = let dmgAdj = max 1 (playerDamage p1 - playerArmor p2)
                       (hits, hpRem) = divMod (playerHP p2) dmgAdj
                    in if hpRem == 0 then hits else hits + 1

beats :: Player -> Player -> Bool
beats p1 p2 = hitsToKill p1 p2 <= hitsToKill p2 p1

combos :: [[Item]]
combos = do
  w <- weapons
  a <- Nothing:(Just <$> armor)
  r1 <- Nothing:(Just <$> rings)
  r2 <- Nothing:(Just <$> rings)
  guard (notJustSame r1 r2)
  pure $ [w] <> maybeToList a <> maybeToList r1 <> maybeToList r2
  where
    notJustSame (Just x) (Just y) = x /= y
    notJustSame _ _ = True

main :: IO ()
main = do
  {-
  let exPlayer = Player 8 5 5
      exBoss = Player 12 7 2
  print $ beats exPlayer exBoss
  print $ hitsToKill exPlayer exBoss
  -}
  let realBoss = Player 100 8 2
      viable = filter ((`beats` realBoss) . playerCharacter) combos
      notViable = filter (not . (`beats` realBoss) . playerCharacter) combos
  print $ minimum $ (sum . fmap itemCost) <$> viable
  print $ maximum $ (sum . fmap itemCost) <$> notViable
