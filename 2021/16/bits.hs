import Data.List (foldl')
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

data PacketType
  = Literal
  | Sum
  | Product
  | Min
  | Max
  | Gt
  | Lt
  | Eq
  deriving Show

data PacketPayload
  = Value Int
  | Packed [Packet]
  deriving Show

data Packet
  = Packet { version :: Int
           , packetType :: PacketType
           , payload :: PacketPayload
           } deriving Show

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

packetTypeP :: Parser PacketType
packetTypeP = do
  code <- bigEndianN 3
  case code of
    0 -> pure Sum
    1 -> pure Product
    2 -> pure Min
    3 -> pure Max
    4 -> pure Literal
    5 -> pure Gt
    6 -> pure Lt
    7 -> pure Eq
    _ -> empty

packet :: Parser Packet
packet = do
  v <- bigEndianN 3
  ty <- packetTypeP
  pay <- case ty of
    Literal -> Value <$> taggedChunks
    _ -> Packed <$> do
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
  pure $ Packet v ty pay

totalVersion :: Packet -> Int
totalVersion p = version p + case payload p of
  Packed ps -> sum $ totalVersion <$> ps
  _ -> 0

eval :: Packet -> Int
eval (Packet _ Literal (Value n)) = n
eval (Packet _ Sum (Packed ps)) = sum $ eval <$> ps
eval (Packet _ Product (Packed ps)) = product $ eval <$> ps
eval (Packet _ Min (Packed ps)) = minimum $ eval <$> ps
eval (Packet _ Max (Packed ps)) = maximum $ eval <$> ps
eval (Packet _ Gt (Packed [p1, p2])) = if eval p1 > eval p2 then 1 else 0
eval (Packet _ Lt (Packed [p1, p2])) = if eval p1 < eval p2 then 1 else 0
eval (Packet _ Eq (Packed [p1, p2])) = if eval p1 == eval p2 then 1 else 0
eval (Packet _ _ _) = error "betcha wish ya had gadts"

main :: IO ()
main = do
  bits <- hex <$> getContents
  let Just (p, _) = runParser packet bits
  print $ totalVersion p
  print $ eval p
