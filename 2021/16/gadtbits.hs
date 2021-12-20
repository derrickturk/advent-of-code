{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}
{-# LANGUAGE RankNTypes, StandaloneDeriving #-}

import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Kind (Type)
import Control.Monad (replicateM, MonadPlus(..))
import Control.Applicative (Alternative(..))

hex :: String -> [Bool]
hex = concatMap hexChar where
  hexChar '0' = [False, False, False, False]
  hexChar '1' = [False, False, False, True]
  hexChar '2' = [False, False, True, False]
  hexChar '3' = [False, False, True, True]
  hexChar '4' = [False, True, False, False]
  hexChar '5' = [False, True, False, True]
  hexChar '6' = [False, True, True, False]
  hexChar '7' = [False, True, True, True]
  hexChar '8' = [True, False, False, False]
  hexChar '9' = [True, False, False, True]
  hexChar 'A' = [True, False, True, False]
  hexChar 'B' = [True, False, True, True]
  hexChar 'C' = [True, True, False, False]
  hexChar 'D' = [True, True, False, True]
  hexChar 'E' = [True, True, True, False]
  hexChar 'F' = [True, True, True, True]
  hexChar _ = []

bigEndian :: [Bool] -> Int
bigEndian = foldl' (\k b -> k * 2 + if b then 1 else 0) 0

newtype Parser a = Parser { runParser :: [Bool] -> Maybe (a, [Bool]) }

instance Functor Parser where
  fmap f p = Parser $ \t -> case runParser p t of
    Just (x, rest) -> Just (f x, rest)
    Nothing -> Nothing

instance Applicative Parser where
  pure x = Parser $ \t -> Just (x, t)
  pf <*> px = Parser $ \t -> do 
    (f, t') <- runParser pf t
    (x, t'') <- runParser px t'
    pure (f x, t'')

instance Monad Parser where
  pa >>= k = Parser $ \t -> do
    (a, t') <- runParser pa t
    runParser (k a) t'

instance Alternative Parser where
  empty = Parser $ const Nothing
  px <|> py = Parser $ \t ->
    case runParser px t of
      Nothing -> runParser py t
      result -> result

instance MonadPlus Parser where

instance MonadFail Parser where
  fail _ = mzero

data PacketKind
  = Flat
  | NestMany
  | NestSome
  | NestTwo
  deriving Show

data SPacketKind :: PacketKind -> Type where
  SFlat :: SPacketKind 'Flat
  SNestMany :: SPacketKind 'NestMany
  SNestSome :: SPacketKind 'NestSome
  SNestTwo :: SPacketKind 'NestTwo

deriving instance Show (SPacketKind k)

data PacketType :: PacketKind -> Type where
  Literal :: PacketType 'Flat
  Sum :: PacketType 'NestMany
  Product :: PacketType 'NestMany
  Min :: PacketType 'NestSome
  Max :: PacketType 'NestSome
  Gt :: PacketType 'NestTwo
  Lt :: PacketType 'NestTwo
  Eq :: PacketType 'NestTwo

deriving instance Show (PacketType k)

data PacketPayload :: PacketKind -> Type where
  Value :: Int -> PacketPayload 'Flat
  PackMany :: [Packet] -> PacketPayload 'NestMany
  PackSome :: NonEmpty Packet -> PacketPayload 'NestSome
  PackTwo :: Packet -> Packet -> PacketPayload 'NestTwo

deriving instance Show (PacketPayload k)

data Packet :: Type where
  Packet :: { version :: Int
            , packetType :: PacketType k
            , payload :: PacketPayload k
            } -> Packet

deriving instance Show Packet

bit :: Parser Bool
bit = Parser $ \bits ->
  case bits of
    [] -> Nothing
    (b:rest) -> Just (b, rest)

bitsN :: Int -> Parser [Bool]
bitsN n = Parser $ \bits ->
  let (pre, post) = splitAt n bits
  in if length pre /= n
       then Nothing
       else Just (pre, post)

bigEndianN :: Int -> Parser Int
bigEndianN n = bigEndian <$> bitsN n

taggedChunks :: Parser Int
taggedChunks = bigEndian . concat <$> taggedChunks' where
  taggedChunks' = do
    cont <- bit
    if cont
      then (:) <$> bitsN 4 <*> taggedChunks'
      else (:) <$> bitsN 4 <*> pure []

newtype ExPacketType = ExPacketType
  { runExPacketType :: forall (r :: Type)
                     . (forall (k :: PacketKind) . SPacketKind k -> PacketType k -> r)
                    -> r
  }

instance Show ExPacketType where
  show (ExPacketType f) = f (\_ ty -> "ExPacketType " <> show ty)

exPacketType :: SPacketKind k -> PacketType k -> ExPacketType
exPacketType k ty = ExPacketType $ \f -> f k ty

packetTypeP :: Parser ExPacketType
packetTypeP = do
  code <- bigEndianN 3
  case code of
    0 -> pure $ exPacketType SNestMany Sum
    1 -> pure $ exPacketType SNestMany Product
    2 -> pure $ exPacketType SNestSome Min
    3 -> pure $ exPacketType SNestSome Max
    4 -> pure $ exPacketType SFlat Literal
    5 -> pure $ exPacketType SNestTwo Gt
    6 -> pure $ exPacketType SNestTwo Lt
    7 -> pure $ exPacketType SNestTwo Eq
    _ -> empty

packet :: Parser Packet
packet = do
  v <- bigEndianN 3
  ty <- packetTypeP
  runExPacketType ty (finishPacket v)
  where
    finishPacket :: Int -> SPacketKind k -> PacketType k -> Parser Packet
    finishPacket v SFlat ty = do
      pay <- Value <$> taggedChunks
      pure $ Packet v ty pay
    finishPacket v SNestMany ty = do
      pay <- PackMany <$> packetList
      pure $ Packet v ty pay
    finishPacket v SNestSome ty = do
      Just pay <- fmap PackSome . nonEmpty <$> packetList
      pure $ Packet v ty pay
    finishPacket v SNestTwo ty = do
      [p1, p2] <- packetList
      pure $ Packet v ty $ PackTwo p1 p2

    packetList :: Parser [Packet]
    packetList = do
      lenIsPackets <- bit
      if lenIsPackets
        then do
          nPackets <- bigEndianN 11
          replicateM nPackets packet
        else do
          nBits <- bigEndianN 15
          contained <- bitsN nBits
          case runParser (some packet) contained of
            Just (subpackets, []) -> pure subpackets
            _ -> empty

totalVersion :: Packet -> Int
totalVersion (Packet v _ (Value _)) = v
totalVersion (Packet v _ (PackMany ps)) = v + (sum $ totalVersion <$> ps)
totalVersion (Packet v _ (PackSome ps)) = v + (sum $ totalVersion <$> ps)
totalVersion (Packet v _ (PackTwo p1 p2)) =
  v + totalVersion p1 + totalVersion p2

eval :: Packet -> Int
eval (Packet _ Literal (Value n)) = n
eval (Packet _ Sum (PackMany ps)) = sum $ eval <$> ps
eval (Packet _ Product (PackMany ps)) = product $ eval <$> ps
eval (Packet _ Min (PackSome ps)) = minimum $ eval <$> ps
eval (Packet _ Max (PackSome ps)) = maximum $ eval <$> ps
eval (Packet _ Gt (PackTwo p1 p2)) = if eval p1 > eval p2 then 1 else 0
eval (Packet _ Lt (PackTwo p1 p2)) = if eval p1 < eval p2 then 1 else 0
eval (Packet _ Eq (PackTwo p1 p2)) = if eval p1 == eval p2 then 1 else 0

main :: IO ()
main = do
  bits <- hex <$> getContents
  let Just (p, _) = runParser packet bits
  print $ totalVersion p
  print $ eval p
