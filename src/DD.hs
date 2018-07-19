{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module DD where

import Data.Diverse hiding (Head)
import Data.Diverse.Many.Internal (Many(..))
import V2 hiding (emptyStore)
import ManyOperations
import Universum hiding (Nat)
import qualified Data.Sequence as S
import Unsafe.Coerce
import Numbers
import Generics.SOP as SOP


finishDD :: forall (effect :: Type -> Type) target (book :: [Type]) (store :: [Type]) b.
  ( store ~ (LiftMaybe (RecipeDepsCalc effect target book))
  , ToS (ListLen (EmptyStore effect target book))
  , HasRecipe effect target book
  , Monad effect
  , (SubSelect effect book (RecipeDeps effect target book) (Many store))
  , (KnownNat (IndexOf (Maybe target) store))
  , EverythingIsApplied effect target book (RecipeDepsCalc effect target book)
  , HasTypes (DepsComputed (Many store)) (RecipeDeps effect target book)
  , SOP.Generic b
  , Code b ~ '[book]
  ) =>
  b -> effect target
finishDD book = do
  let
    store = emptyStore (Proxy @effect) (Proxy @target) (Proxy @book)
  s <- bake (extractBook book) store (Proxy @target)
  pure $ getTyped @_ @target (DepsComputed s)

type family LiftMaybe (l :: [k]) :: [k] where
  LiftMaybe (head ': tail) = Maybe head ': (LiftMaybe tail)
  LiftMaybe '[] = '[]

type EmptyStore effect target book = (LiftMaybe (RecipeDepsCalc effect target book))

--  - first calculate the required storage for state via type family
--  - fill it with Nothing
--  - use as required
emptyStore :: forall effect target book.
  (ToS (ListLen (EmptyStore effect target book))) =>
    Proxy effect -> Proxy target -> Proxy book -> Many (EmptyStore effect target book)
emptyStore _ _ _ =
  unsafeCoerce $ S.fromFunction len (const Nothing)
  where len :: Int = toLen (Proxy @(LiftMaybe (RecipeDepsCalc effect target book)))

instance KnownNat (IndexOf (Maybe a) deps) => HasType (DepsComputed (Many deps)) a where
  getTyped (DepsComputed m) = fromMaybe (error "No element of this type available. This should not happen, it should have been produced by an earlier bake. Please file a bug.") $ grabFirst m
  setTyped element (DepsComputed m)= DepsComputed $ replaceFirst' m $ Just element

instance KnownNat (IndexOf a deps) => HasType (Many deps) a where
  getTyped = grabFirst
  setTyped = flip replaceFirst'
