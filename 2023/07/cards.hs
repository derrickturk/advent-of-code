import Data.Char (digitToInt, isDigit)
import Data.List (foldl', sort)
import Text.Read (readMaybe)

import qualified Data.Map.Strict as M

data Card
  = Number Int
  | T
  | J
  | Q
  | K
  | A
  deriving (Show, Eq, Ord)

newtype Hand c = Hand { unHand {- me, you brigand -} :: (M.Map c Int, [c]) }
  deriving (Show, Eq)

makeHand :: Ord c => [c] -> Hand c
makeHand cs =
  let bump Nothing = Just 1
      bump (Just c) = Just (c + 1)
      m = foldl' (flip $ M.alter bump) M.empty cs
   in Hand (m, cs)

data HandKind
  = HighCard
  | OnePair
  | TwoPair
  | Three
  | FullHouse
  | Four
  | Five
  deriving (Show, Eq, Ord)

class Classify c where
  classify :: Hand c -> HandKind

instance Classify Card where
  classify (Hand (m, _)) = case sort (M.elems m) of
    [5] -> Five
    [1, 4] -> Four
    [2, 3] -> FullHouse
    [1, 1, 3] -> Three
    [1, 2, 2] -> TwoPair
    [1, 1, 1, 2] -> OnePair
    _ -> HighCard

instance (Ord c, Classify c) => Ord (Hand c) where
  compare a@(Hand (_, acs)) b@(Hand (_, bcs)) =
    case compare (classify a) (classify b) of
      EQ -> compare acs bcs
      r -> r

parseCard :: Char -> Maybe Card
parseCard 'T' = Just T
parseCard 'J' = Just J
parseCard 'Q' = Just Q
parseCard 'K' = Just K
parseCard 'A' = Just A
parseCard c
  | isDigit c = let v = digitToInt c
                 in if v >= 2 && v <= 9 then Just (Number v) else Nothing
  | otherwise = Nothing

parseHand :: String -> Maybe (Hand Card)
parseHand = fmap makeHand . traverse parseCard

parseHandBid :: String -> Maybe (Hand Card, Int)
parseHandBid l = do
  [hand, bid] <- pure $ words l
  hand' <- parseHand hand
  bid' <- readMaybe bid
  return (hand', bid')

score :: (Ord c, Classify c) => [(Hand c, Int)] -> Int
score = sum . fmap (\(r, (_, b)) -> r * b) . zip [1..] . sort

newtype JCard = JCard { unJCard :: Card }
  deriving (Show, Eq)

instance Ord JCard where
  compare (JCard J) (JCard J) = EQ
  compare (JCard J) _ = LT
  compare _ (JCard J) = GT
  compare (JCard c1) (JCard c2) = compare c1 c2

instance Classify JCard where
  classify (Hand (m, _)) =
    case sort (M.elems $ M.filterWithKey (\k _ -> k /= JCard J) m) of
     [] -> Five
     [_] -> Five
     [1, _] -> Four
     [2, _] -> FullHouse
     [1, 1, _] -> Three
     [1, 2, 2] -> TwoPair
     [1, 1, 1, _] -> OnePair
     _ -> HighCard

jokerize :: Hand Card -> Hand JCard
jokerize (Hand (_, cs)) = makeHand $ JCard <$> cs

main :: IO ()
main = do
  Just hands <- traverse parseHandBid . lines <$> getContents
  print $ score hands
  print $ score $ (\(h, b) -> (jokerize h, b)) <$> hands
