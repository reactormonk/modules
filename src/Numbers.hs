{-# LANGUAGE PolyKinds #-}
module Numbers where

import Universum hiding (Nat)

type family ListLen (l1 :: [k]) :: Nat where
  ListLen (_ ': tail) = S (ListLen tail)
  ListLen '[] = Z

data Nat = Z | S Nat

data SNat (n :: Nat) where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

class ToS (n :: Nat) where
  toS :: SNat n

instance ToS Z where
  toS = SZ

instance ToS n => ToS (S n) where
  toS = SS toS

toInt :: SNat n -> Int
toInt SZ = 0
toInt (SS s) = 1 + toInt s

toLen :: forall l. ToS (ListLen l) => Proxy l -> Int
toLen _ = toInt $ (toS @(ListLen l))
