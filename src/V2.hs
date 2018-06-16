{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module V2 where

import ManyOperations
import Universum hiding (Nat, Type)
import Data.Diverse
import qualified Data.Sequence as S
import Data.Diverse.Many.Internal (Many(..))
import Unsafe.Coerce
import Numbers
import GHC.TypeLits as Lits

newtype Recipe (effect :: * -> *) target (deps :: [*]) = Recipe { runRecipe :: forall a. RecipeDepsHasTypes deps a => a -> effect target }

pureRecipe :: Applicative effect => target -> Recipe effect target '[]
pureRecipe target = Recipe $ \_ -> pure target

--  - first calculate the required storage for state via type family
--  - fill it with Nothing
--  - use as required

-- Not really in order, but doesn't matter here
type family AddLists (l1 :: [k]) (l2 :: [k]) :: [k] where
  AddLists l1 '[] = l1
  AddLists '[] l2 = l2
  AddLists l1 (h2 ': t2) = AddLists (h2 ': l1) t2

type family RecipeDeps (effect :: * -> *) (target :: *) (book :: [*]) :: [*] where
  RecipeDeps effect target ((Recipe effect target deps) ': tBook) = deps
  RecipeDeps effect target (hBook ': tBook) = RecipeDeps effect target tBook
  RecipeDeps effect target '[] = DefaultRecipeDeps effect target

type family RecipeDepsRec (effect :: * -> *) (target :: *) (book :: [*]) (deps :: [*]) :: [*] where
  RecipeDepsRec effect target book (hDeps ': tDeps) = AddLists
    (RecipeDepsRec effect target book (RecipeDeps effect hDeps book))
    (AddLists
      '[hDeps]
      (RecipeDepsRec effect target book tDeps))
  RecipeDepsRec effect target book '[] = '[]

type family RecipeDepsHasTypes (deps :: [*]) (s :: *) :: Constraint where
  RecipeDepsHasTypes (h ': t) s = (HasType h s, RecipeDepsHasTypes t s)
  RecipeDepsHasTypes '[] s = ()

class HasType a s where
  getTyped :: s -> a
  setTyped :: a -> s -> s

newtype PassedAsDeps deps = PassedAsDeps deps

instance KnownNat (IndexOf (Maybe a) deps) => HasType a (PassedAsDeps (Many deps)) where
  getTyped (PassedAsDeps m) = fromMaybe (error "No element of this type available. This should not happen, it should have been produced by an earlier cook. Please file a bug.") $ grabFirst m
  setTyped elem (PassedAsDeps m)= PassedAsDeps $ replaceFirst' m $ Just elem

instance KnownNat (IndexOf a deps) => HasType a (Many deps) where
  getTyped = grabFirst
  setTyped = flip replaceFirst'

class HasRecipe (effect :: * -> *) target (book :: [*]) where
  recipe :: Many book -> Recipe effect target (RecipeDeps effect target book)

instance {-# OVERLAPPING #-} (deps ~ RecipeDeps effect target ((Recipe effect target deps) ': tail)) =>
  HasRecipe effect target ((Recipe effect target deps) ': tail) where
    recipe :: Many ((Recipe effect target deps) ': tail) -> Recipe effect target (RecipeDeps effect target ((Recipe effect target deps) ': tail))
    recipe list = front list

instance ((RecipeDeps effect target tail) ~ (RecipeDeps effect target (head ': tail)), HasRecipe effect target tail) =>
  HasRecipe effect target (head ': tail) where
    recipe :: Many (head ': tail) -> Recipe effect target (RecipeDeps effect target tail)
    recipe list = recipe $ aft list

type family LiftMaybe (l :: [k]) :: [k] where
  LiftMaybe (head ': tail) = Maybe head ': (LiftMaybe tail)
  LiftMaybe '[] = '[]

type EmptyStore effect target book = (LiftMaybe (RecipeDepsCalc effect target book))

emptyStore :: forall effect target book.
  (ToS (ListLen (EmptyStore effect target book))) =>
    Proxy effect -> Proxy target -> Proxy book -> Many (EmptyStore effect target book)
emptyStore _ _ _ =
  unsafeCoerce $ S.fromFunction len (const Nothing)
  where len :: Int = toLen (Proxy @(LiftMaybe (RecipeDepsCalc effect target book)))

findOrUpdate :: forall elem store f. (HasType (Maybe elem) store, Applicative f) => store -> f elem -> f (store, elem)
findOrUpdate pot elem =
  case getTyped pot :: Maybe elem of
    Just x -> pure (pot, x)
    Nothing -> do
      x <- elem
      pure (setTyped (Just x) pot, x)

class SubSelect (effect :: * -> *) (book :: [*]) (deps :: [*]) state where
  subselect :: Many book -> state -> Proxy deps -> effect state

instance forall effect book state dep depTail.
  (Monad effect, CanCook book state effect dep, SubSelect effect book depTail state) =>
  SubSelect effect book (dep ': depTail) state where
    subselect :: Many book -> state -> Proxy deps -> effect state
    subselect book s1 _ = do
      s2 <- cook book s1 (Proxy :: Proxy dep)
      s3 <- subselect book s2 (Proxy :: Proxy depTail)
      pure $ s3

instance (Monad effect) => SubSelect effect book '[] state where
  subselect _ state _ = pure state

class CanCook (book :: [*]) state (effect :: * -> *) target where
  cook :: Many book -> state -> Proxy target -> effect state

instance forall effect target book state.
  (HasRecipe effect target book,
   SubSelect effect book (RecipeDeps effect target book) state,
   Monad effect,
   RecipeDepsHasTypes (RecipeDeps effect target book) (PassedAsDeps state),
   HasType (Maybe target) state) =>
  CanCook book state effect target where
    cook :: Many book -> state -> Proxy target -> effect state
    cook book s1 (Proxy :: Proxy target)= do
      let
        s2 :: effect state
        s2 = subselect book s1 (Proxy @(RecipeDeps effect target book))
        r :: Recipe effect target (RecipeDeps effect target book)
        r = recipe book
        res :: state -> effect (state, target)
        res s2 = findOrUpdate (s2 :: state) (runRecipe r $ PassedAsDeps s2)
      s2r :: state <- s2
      (s3, t) <- (res s2r) :: effect (state, target)
      pure $ s3

type family Contains (target :: *) (store :: [*]) :: Bool where
  Contains target (target ': t) = 'True
  Contains target (h ': t) = Contains target t
  Contains target '[] = 'False

type family EverythingIsAppliedTypeError (bool :: Bool) (s :: *) (b :: [*]) :: Constraint where
  EverythingIsAppliedTypeError 'True s b = ()
  EverythingIsAppliedTypeError 'False s b = TypeError ('Text "There is a recipe for " ':<>: 'ShowType s ':<>: 'Text " but it's not being used anywhere in the current pot: " ':<>: 'ShowType b)

type family EverythingIsApplied (effect :: * -> *) target (book :: [*]) (store :: [*]) :: Constraint where
  EverythingIsApplied effect target ((Recipe effect head _) ': tBook) store = (EverythingIsAppliedTypeError (Contains head store) head store, EverythingIsApplied effect target tBook store)
  EverythingIsApplied effect target (head ': tBook) store = TypeError ('Text "The type " ':<>: 'ShowType head ':<>: 'Text " is not a Recipe")
  EverythingIsApplied effect target '[] store = ()

finish :: forall (effect :: * -> *) target (book :: [*]) store.
  ( Monad effect
  , Monoid store
  , SubSelect effect book (RecipeDeps effect target book) store
  , HasRecipe effect target book
  , HasType (Maybe target) store
  , RecipeDepsHasTypes (RecipeDeps effect target book) (PassedAsDeps store)
  ) =>
  Many book -> Proxy store -> effect target
finish book _ = do
  let
  state <- cook book (mempty :: store) (Proxy @target)
  pure $ fromMaybe (error "No element of this type available. This should not happen, it should have been produced by an earlier cook. Please file a bug.") $ getTyped @(Maybe target) state

type RecipeDepsCalc effect target book = target ': (RecipeDepsRec effect target book (RecipeDeps effect target book))

finishDD :: forall (effect :: * -> *) target (book :: [*]) (store :: [*]).
  ( store ~ (LiftMaybe (RecipeDepsCalc effect target book))
  , ToS (ListLen (EmptyStore effect target book))
  , HasRecipe effect target book
  , Monad effect
  , (SubSelect effect book (RecipeDeps effect target book) (Many store))
  , (KnownNat (IndexOf (Maybe target) store))
  , EverythingIsApplied effect target book (RecipeDepsCalc effect target book)
  , RecipeDepsHasTypes (RecipeDeps effect target book) (PassedAsDeps (Many store))
  ) =>
  Many book -> effect target
finishDD book = do
  let
    store = emptyStore (Proxy @effect) (Proxy @target) (Proxy @book)
  state <- cook book store (Proxy @target)
  pure $ getTyped @target (PassedAsDeps state)

class DefaultRecipe (effect :: * -> *) target where
  type DefaultRecipeDeps effect target :: [*]
  def :: Recipe effect target (DefaultRecipeDeps effect target)

instance DefaultRecipe effect target => HasRecipe effect target '[] where
  recipe _ = def
