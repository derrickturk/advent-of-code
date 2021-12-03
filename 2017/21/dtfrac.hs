{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, TypeApplications #-}

-- this one's going on the cutting room floor...

import Data.Kind (Type)
import Data.Maybe (fromJust)
import Control.Monad ((>=>))

data Nat
  = Z
  | S Nat
  deriving Show

type family Twice (n :: Nat) where
  Twice Z = Z
  Twice (S n) = S (S (Twice n))

data Fin (n :: Nat) where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)

deriving instance Show (Fin n)

data SNat (n :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

deriving instance Show (SNat n)

type One = 'S 'Z
type Two = 'S ('S 'Z)
type Three = 'S ('S ('S 'Z))

one :: SNat One
one = SS SZ

two :: SNat Two
two = SS one

three :: SNat Three
three = SS two

infixr 5 :>
data Vec (n :: Nat) (a :: Type) where
  VNil :: Vec 'Z a
  (:>) :: a -> Vec n a -> Vec ('S n) a

deriving instance Show a => Show (Vec n a)

fromList :: SNat n -> [a] -> Maybe (Vec n a)
fromList SZ [] = Just VNil
fromList (SS n) (x:xs) = (x:>) <$> fromList n xs
fromList _ _ = Nothing

newtype Grid (n :: Nat) (m :: Nat) =
  Grid { unGrid :: Vec m (Vec m (Vec n (Vec n Bool))) }
  deriving Show

parseGrid :: SNat n -> [String] -> Maybe (Vec n (Vec n Bool))
parseGrid n = traverse parseRow >=> fromList n where
  parseRow = traverse parseCell >=> fromList n
  parseCell '#' = Just True
  parseCell '.' = Just False
  parseCell _ = Nothing

solo :: Vec n (Vec n Bool) -> Grid n One
solo = Grid . (:> VNil) . (:> VNil)

startPattern :: Grid Three One
startPattern = solo $ fromJust $ parseGrid three
  [ ".#."
  , "..#"
  , "###"
  ]

main :: IO ()
main = do
  pure ()
