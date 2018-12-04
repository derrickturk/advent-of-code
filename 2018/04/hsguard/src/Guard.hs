module Guard (
    GuardId(..)
  , DateTime(..)
  , Action(..)
  , LogEntry(..)
  , RawAction(..)
  , RawLogEntry(..)
  , cookLogEntries
  , minutesDiff
  , sleepMinutes
  , sleepByMinute
  , maximumValue
  , maximumValueMaybe
  , guardIds
) where

import Data.List (sort, maximumBy, foldl')
import Control.Monad (foldM)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

newtype GuardId = GuardId { getGuardId :: Int }
  deriving (Show, Eq, Ord)

data DateTime = DateTime { year :: Int
                         , month :: Int
                         , day :: Int
                         , hour :: Int
                         , minute :: Int
                         } deriving (Show, Eq, Ord)

data Action = BeginShift
            | Sleep
            | Wake
            deriving (Show, Eq, Ord)

data LogEntry = LogEntry { guardId :: GuardId
                         , dateTime :: DateTime
                         , action :: Action
                         } deriving (Show, Eq)

data RawAction = RawBeginShift GuardId
               | RawSleep
               | RawWake
               deriving (Show, Eq, Ord)

data RawLogEntry = RawLogEntry DateTime RawAction
                 deriving (Show, Eq, Ord)

cookLogEntries :: [RawLogEntry] -> Maybe [LogEntry]
cookLogEntries = cook Nothing . sort where
  cook _ [] = Just []
  cook _ ((RawLogEntry dt (RawBeginShift g)):rest) =
    (:) <$> pure (LogEntry g dt BeginShift) <*> cook (Just g) rest
  cook (Just g) ((RawLogEntry dt RawSleep):rest) =
    (:) <$> pure (LogEntry g dt Sleep) <*> cook (Just g) rest
  cook (Just g) ((RawLogEntry dt RawWake):rest) =
    (:) <$> pure (LogEntry g dt Wake) <*> cook (Just g) rest
  cook Nothing _ = Nothing

minutesDiff :: DateTime -> DateTime -> Int
minutesDiff (DateTime y2 mo2 d2 h2 m2) (DateTime y1 mo1 d1 h1 m1) =
  (y2 - y1) * 365 * 1440 +
  (mo2 - mo1) * 30 * 1440 +
  (d2 - d1) * 1440 +
  (h2 - h1) * 60 +
  (m2 - m1)

sleepMinutes :: [LogEntry] -> Maybe (M.Map GuardId Int)
sleepMinutes = fmap fst . foldM update (M.empty, M.empty) where
  update (mins, begins) (LogEntry g dt Sleep) =
    pure (mins, M.insert g dt begins)
  update (mins, begins) (LogEntry g dt Wake) = do
    slept <- M.lookup g begins
    pure (M.insertWith (+) g (dt `minutesDiff` slept) mins, begins)
  update (mins, begins) _ = pure (mins, begins)

sleepByMinute :: GuardId -> [LogEntry] -> Maybe (M.Map Int Int)
sleepByMinute g =
  fmap fst . foldM update (M.empty, Nothing) . filter (isGuard g)
  where
    update (mins, begins) (LogEntry _ dt Sleep) = pure (mins, Just dt)
    update (mins, begins) (LogEntry _ dt Wake) = do
      slept <- begins
      let mins' = foldl' countMinute mins [minute slept .. (minute dt - 1)]
      pure (mins', begins)
    update (mins, begins) _ = pure (mins, begins)
    countMinute mins m = M.insertWith (+) m 1 mins
    isGuard g e = g == guardId e

maximumValue :: (Ord k, Ord a) => M.Map k a -> (k, a)
maximumValue = maximumBy (\x y -> compare (snd x) (snd y)) . M.toList

maximumValueMaybe :: (Ord k, Ord a) => M.Map k a -> Maybe (k, a)
maximumValueMaybe m = if M.null m then Nothing else Just (maximumValue m)

guardIds :: [LogEntry] -> [GuardId]
guardIds = S.toList . S.fromList . fmap guardId
