{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ManyOperations where

import Data.Diverse
import Data.Diverse.Many
import Data.Diverse.Many.Internal (Many(..))
import qualified Data.Sequence as S
-- import Universum hiding (Any)
import Prelude
import Data.Proxy
import GHC.TypeLits as Lits
import GHC.Exts (Any)
import Unsafe.Coerce

replaceFirst' :: forall x xs. KnownNat (IndexOf x xs) => Many xs -> x -> Many xs
replaceFirst' (Many xs) x = Many $ replace_ @(IndexOf x xs) xs (unsafeCoerce x)

replace_ :: forall (n :: Nat). KnownNat n => S.Seq Any -> Any -> S.Seq Any
replace_ xs x = S.update i x xs
  where i = fromInteger (Lits.natVal @n (Proxy @n))

grabFirst :: forall x xs. KnownNat (IndexOf x xs) => Many xs -> x
grabFirst (Many xs) = unsafeCoerce $ grab_ @(IndexOf x xs) xs

grab_ :: forall n. KnownNat n => S.Seq Any -> Any
grab_ xs = let !x = S.index xs i in x -- forcing x to avoid storing Seq in thunk
  where i = fromInteger (Lits.natVal @n Proxy)
