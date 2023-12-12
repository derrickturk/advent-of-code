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

newtype Hand = Hand { unHand {- me, you brigand -} :: (M.Map Card Int, [Card]) }
  deriving (Show, Eq)

makeHand :: [Card] -> Hand
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

classify :: Hand -> HandKind
classify (Hand (m, _)) = case sort (M.elems m) of
  [5] -> Five
  [1, 4] -> Four
  [2, 3] -> FullHouse
  [1, 1, 3] -> Three
  [1, 2, 2] -> TwoPair
  [1, 1, 1, 2] -> OnePair
  _ -> HighCard

instance Ord Hand where
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

parseHand :: String -> Maybe Hand
parseHand = fmap makeHand . traverse parseCard

parseHandBid :: String -> Maybe (Hand, Int)
parseHandBid l = do
  [hand, bid] <- pure $ words l
  hand' <- parseHand hand
  bid' <- readMaybe bid
  return (hand', bid')

score :: [(Hand, Int)] -> Int
score = sum . fmap (\(r, (_, b)) -> r * b) . zip [1..] . sort

main :: IO ()
main = do
  Just hands <- traverse parseHandBid . lines <$> getContents
  print $ score hands
