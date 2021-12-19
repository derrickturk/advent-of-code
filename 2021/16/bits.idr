import Data.List

hex : String -> List Bool
hex = concatMap hexChar . unpack where
  hexChar: Char -> List Bool
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

bigEndian : List Bool -> Int
bigEndian = foldl f 0 where
  f : Int -> Bool -> Int
  f k b = k * 2 + if b then 1 else 0

data Parser a = MkParser (List Bool -> Maybe (a, List Bool))

runParser : Parser a -> List Bool -> Maybe (a, List Bool)
runParser (MkParser p) = p

Functor Parser where
  map f (MkParser p) = MkParser $ \t => case p t of
    Just (x, rest) => Just (f x, rest)
    Nothing => Nothing

Applicative Parser where
  pure x = MkParser $ \t => Just (x, t)
  pf <*> px = MkParser $ \t => do 
    (f, t') <- runParser pf t
    (x, t'') <- runParser px t'
    pure (f x, t'')

Monad Parser where
  pa >>= k = MkParser $ \t => do
    (a, t') <- runParser pa t
    runParser (k a) t'

Alternative Parser where
  empty = MkParser $ const Nothing
  px <|> py = MkParser $ \t =>
    case runParser px t of
      Nothing => runParser py t
      result => result

replicateM : Applicative f => Nat -> f a -> f (List a)
replicateM 0 _ = pure []
replicateM (S n) m = (::) <$> m <*> replicateM n m

many : Parser a -> Parser (List a)
many p = MkParser $ \t => case runParser p t of
  Nothing => Just ([], t)
  Just (x, t') => case runParser (many p) t' of
                    Just (xs, t'') => Just (x::xs, t'')
                    Nothing => Nothing

some : Parser a -> Parser (List a)
some p = MkParser $ \t => case runParser p t of
  Nothing => Nothing
  Just (x, t') => case runParser (many p) t' of
                    Just (xs, t'') => Just (x::xs, t'')
                    Nothing => Nothing

data PacketKind
  = Flat
  | NestMany
  | NestSome
  | NestTwo

data PacketType : PacketKind -> Type where
  Literal : PacketType Flat
  Sum : PacketType NestMany
  Product : PacketType NestMany
  Min : PacketType NestSome
  Max : PacketType NestSome
  Gt : PacketType NestTwo
  Lt : PacketType NestTwo
  Eq : PacketType NestTwo

mutual
  data PacketPayload : PacketKind -> Type where
    Value : Int -> PacketPayload Flat
    PackMany : List Packet -> PacketPayload NestMany
    PackSome : (ps: List Packet)
            -> {auto 0 _ : NonEmpty ps}
            -> PacketPayload NestSome
    PackTwo : Packet -> Packet -> PacketPayload NestTwo

  record Packet where
    constructor MkPacket
    version : Int
    packetType : PacketType kind
    payload : PacketPayload kind

bit : Parser Bool
bit = MkParser $ \bits =>
  case bits of
    [] => Nothing
    (b::rest) => Just (b, rest)

bitsN : Nat -> Parser (List Bool)
bitsN n = MkParser $ \bits =>
  let (pre, post) = splitAt n bits
  in if length pre /= n
       then Nothing
       else Just (pre, post)

bigEndianN : Nat -> Parser Int
bigEndianN n = bigEndian <$> bitsN n

taggedChunks : Parser Int
taggedChunks = bigEndian . concat <$> taggedChunks' where
  taggedChunks' : Parser (List (List Bool))
  taggedChunks' = do
    cont <- bit
    if cont
      then (::) <$> bitsN 4 <*> taggedChunks'
      else (::) <$> bitsN 4 <*> pure []

packetTypeP : Parser (kind : PacketKind ** PacketType kind)
packetTypeP = do
  code <- bigEndianN 3
  case code of
    0 => pure (NestMany ** Sum)
    1 => pure (NestMany ** Product)
    2 => pure (NestSome ** Min)
    3 => pure (NestSome ** Max)
    4 => pure (Flat ** Literal)
    5 => pure (NestTwo ** Gt)
    6 => pure (NestTwo ** Lt)
    7 => pure (NestTwo ** Eq)
    _ => empty

packet : Parser Packet
packet = do
  v <- bigEndianN 3
  (kind ** ty) <- packetTypeP
  pay <- case kind of
    Flat => Value <$> taggedChunks
    NestMany => PackMany <$> packetList
    NestSome => do
      nested <- packetList
      case nested of
        (_::_) => pure $ PackSome nested
        _ => empty
    NestTwo => do
      nested <- packetList
      case nested of
        [p1, p2] => pure $ PackTwo p1 p2
        _ => empty
  pure $ MkPacket v ty pay
  where
    packetList : Parser (List Packet)
    packetList = do
      lenIsPackets <- bit
      if lenIsPackets
        then do
          nPackets <- cast <$> bigEndianN 11
          replicateM nPackets packet
        else do
          nBits <- cast <$> bigEndianN 15
          contained <- bitsN nBits
          case runParser (some packet) contained of
            Just (subpackets, []) => pure subpackets
            _ => empty

totalVersion : Packet -> Int
totalVersion (MkPacket v _ (PackMany ps)) = v + sum (totalVersion <$> ps)
totalVersion (MkPacket v _ (PackSome ps)) = v + sum (totalVersion <$> ps)
totalVersion (MkPacket v _ (PackTwo p1 p2)) =
  v + totalVersion p1 + totalVersion p2
totalVersion (MkPacket v _ _) = v

minimum : Ord a => (l : List a) -> {auto 0 _ : NonEmpty l} -> a
minimum = foldl1 min

minimum' : List Int -> Int
minimum' xs = case xs of
  [] => 0
  (_::_) => minimum xs

maximum : Ord a => (l : List a) -> {auto 0 _ : NonEmpty l} -> a
maximum = foldl1 min

maximum' : List Int -> Int
maximum' xs = case xs of
  [] => 0
  (_::_) => maximum xs

eval : Packet -> Int
eval (MkPacket _ Literal (Value n)) = n
eval (MkPacket _ Sum (PackMany ps)) = sum $ eval <$> ps
eval (MkPacket _ Product (PackMany ps)) = product $ eval <$> ps
eval (MkPacket _ Min (PackSome ps)) = minimum' $ eval <$> ps
eval (MkPacket _ Max (PackSome ps)) = maximum' $ eval <$> ps
eval (MkPacket _ Gt (PackTwo p1 p2)) = if eval p1 > eval p2 then 1 else 0
eval (MkPacket _ Lt (PackTwo p1 p2)) = if eval p1 < eval p2 then 1 else 0
eval (MkPacket _ Eq (PackTwo p1 p2)) = if eval p1 == eval p2 then 1 else 0

main : IO ()
main = do
  bits <- hex <$> getLine
  case runParser packet bits of
    Just (p, _) => do
      printLn $ totalVersion p
      printLn $ eval p
    _ => putStrLn "nope"
