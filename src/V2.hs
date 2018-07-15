{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module V2 where

import Universum hiding (Nat)
import Unsafe.Coerce
import GHC.TypeLits as Lits
import Generics.SOP as SOP

newtype Recipe (effect :: Type -> Type) target (deps :: [Type]) =
  Recipe { runRecipe :: forall depStore. HasTypes deps depStore => depStore -> effect target }

pureRecipe :: Applicative effect => target -> Recipe effect target '[]
pureRecipe target = Recipe $ \_ -> pure target

-- Not really in order, but doesn't matter here
type family AddLists (l1 :: [k]) (l2 :: [k]) :: [k] where
  AddLists l1 '[] = l1
  AddLists '[] l2 = l2
  AddLists l1 (h2 ': t2) = AddLists (h2 ': l1) t2

type family RecipeDeps (effect :: Type -> Type) (target :: Type) (book :: [Type]) :: [Type] where
  RecipeDeps effect target ((Recipe effect target deps) ': tBook) = deps
  RecipeDeps effect target (hBook ': tBook) = RecipeDeps effect target tBook
  RecipeDeps effect target '[] = DefaultRecipeDeps effect target

type family RecipeDepsRec (effect :: Type -> Type) (target :: Type) (book :: [Type]) (deps :: [Type]) :: [Type] where
  RecipeDepsRec effect target book (hDeps ': tDeps) = AddLists
    (RecipeDepsRec effect target book (RecipeDeps effect hDeps book))
    (AddLists
      '[hDeps]
      (RecipeDepsRec effect target book tDeps))
  RecipeDepsRec effect target book '[] = '[]

type family HasTypes (deps :: [Type]) (s :: Type) :: Constraint where
  HasTypes (h ': t) s = (HasType h s, HasTypes t s)
  HasTypes '[] s = ()

class HasType a s where
  getTyped :: s -> a
  setTyped :: a -> s -> s

-- Dependendencies already computed
newtype DepsComputed deps = DepsComputed deps

class HasRecipe (effect :: Type -> Type) target (book :: [Type]) where
  recipe :: NP I book -> Recipe effect target (RecipeDeps effect target book)

instance {-# OVERLAPPING #-} (deps ~ RecipeDeps effect target ((Recipe effect target deps) ': tail)) =>
  HasRecipe effect target ((Recipe effect target deps) ': tail) where
    recipe :: NP I ((Recipe effect target deps) ': tail) -> Recipe effect target (RecipeDeps effect target ((Recipe effect target deps) ': tail))
    recipe (l :* _) = unI $ l

instance ((RecipeDeps effect target tail) ~ (RecipeDeps effect target (head ': tail)), HasRecipe effect target tail) =>
  HasRecipe effect target (head ': tail) where
    recipe :: NP I (head ': tail) -> Recipe effect target (RecipeDeps effect target tail)
    recipe (_ :* t) = recipe $ t

findOrUpdate :: forall element store f. (HasType (Maybe element) store, Applicative f) => store -> f element -> f (store, element)
findOrUpdate pot element =
  case getTyped pot :: Maybe element of
    Just x -> pure (pot, x)
    Nothing -> do
      x <- element
      pure (setTyped (Just x) pot, x)

class SubSelect (effect :: Type -> Type) (book :: [Type]) (deps :: [Type]) state where
  subselect :: NP I book -> state -> Proxy deps -> effect state

instance forall effect book state dep depTail.
  (Monad effect, CanBake book state effect dep, SubSelect effect book depTail state) =>
  SubSelect effect book (dep ': depTail) state where
    subselect :: NP I book -> state -> Proxy deps -> effect state
    subselect book s1 _ = do
      s2 <- bake book s1 (Proxy :: Proxy dep)
      s3 <- subselect book s2 (Proxy :: Proxy depTail)
      pure $ s3

instance (Monad effect) => SubSelect effect book '[] state where
  subselect _ s _ = pure s

class CanBake (book :: [Type]) state (effect :: Type -> Type) target where
  bake :: NP I book -> state -> Proxy target -> effect state

instance forall effect target book state.
  (HasRecipe effect target book,
   SubSelect effect book (RecipeDeps effect target book) state,
   Monad effect,
   HasTypes (RecipeDeps effect target book) (DepsComputed state),
   HasType (Maybe target) state) =>
  CanBake book state effect target where
    {-# NOINLINE bake #-}
    bake :: NP I book -> state -> Proxy target -> effect state
    bake book s1 (Proxy :: Proxy target)= do
      let
        s2 :: effect state
        s2 = subselect book s1 (Proxy @(RecipeDeps effect target book))
        r :: Recipe effect target (RecipeDeps effect target book)
        r = recipe book
        res :: state -> effect (state, target)
        res s2f = findOrUpdate (s2f :: state) (runRecipe r $ DepsComputed s2f)
      s2r :: state <- s2
      (s3, _) <- (res s2r) :: effect (state, target)
      pure $ s3

type family Contains (target :: Type) (store :: [Type]) :: Bool where
  Contains target (target ': t) = 'True
  Contains target (h ': t) = Contains target t
  Contains target '[] = 'False

type family EverythingIsAppliedTypeError (bool :: Bool) (s :: Type) (b :: [Type]) :: Constraint where
  EverythingIsAppliedTypeError 'True s b = ()
  EverythingIsAppliedTypeError 'False s b = TypeError ('Text "There is a recipe for " ':<>: 'ShowType s ':<>: 'Text " but it's not being used anywhere in the current pot: " ':<>: 'ShowType b)

type family EverythingIsApplied (effect :: Type -> Type) target (book :: [Type]) (store :: [Type]) :: Constraint where
  EverythingIsApplied effect target ((Recipe effect head _) ': tBook) store = (EverythingIsAppliedTypeError (Contains head store) head store, EverythingIsApplied effect target tBook store)
  EverythingIsApplied effect target (head ': tBook) store = TypeError ('Text "The type " ':<>: 'ShowType head ':<>: 'Text " is not a Recipe")
  EverythingIsApplied effect target '[] store = ()

finishStore :: forall (effect :: Type -> Type) target (book :: [Type]) store b.
  ( Monad effect
  , Monoid store
  , SubSelect effect book (RecipeDeps effect target book) store
  , HasRecipe effect target book
  , HasType (Maybe target) store
  , HasTypes (RecipeDeps effect target book) (DepsComputed store)
  , EverythingIsApplied effect target book (RecipeDepsCalc effect target book)
  , SOP.Generic b
  , Code b ~ '[book]
  ) =>
  b -> Proxy store -> effect target
finishStore book _ = do
  let
  s <- bake (extractBook book) (mempty :: store) (Proxy @target)
  pure $ fromMaybe (error "No element of this type available. This should not happen, it should have been produced by an earlier bake. Please file a bug.") $ getTyped @(Maybe target) s

type RecipeDepsCalc effect target book = target ': (RecipeDepsRec effect target book (RecipeDeps effect target book))

class DefaultRecipe (effect :: Type -> Type) target where
  type DefaultRecipeDeps effect target :: [Type]
  def :: Recipe effect target (DefaultRecipeDeps effect target)

instance DefaultRecipe effect target => HasRecipe effect target '[] where
  recipe _ = def

newtype PureDepHolder book = PureDepHolder (NP I book)

instance forall target (book :: [Type]) (deps :: [Type]).
  ( deps ~ (RecipeDeps Identity target book)
  , HasRecipe Identity target book
  , HasTypes deps (DepsComputed (PureDepHolder book))
  , SubSelect Identity book deps (PureDepHolder book)
  ) => HasType (Maybe target) (PureDepHolder book) where
  getTyped (PureDepHolder book) = Just $ runIdentity $ do
    let
      r :: Recipe Identity target deps
      r = recipe book
    deps <- subselect book (PureDepHolder book) (Proxy @deps)
    runRecipe r $ DepsComputed deps
  setTyped _ x = x

instance forall target (book :: [Type]) (deps :: [Type]).
  ( deps ~ (RecipeDeps Identity target book)
  , HasRecipe Identity target book
  , HasTypes deps (DepsComputed (PureDepHolder book))
  , SubSelect Identity book deps (PureDepHolder book)
  ) => HasType target (DepsComputed (PureDepHolder book)) where
  getTyped (DepsComputed (PureDepHolder book)) = runIdentity $ do
    let
      r :: Recipe Identity target deps
      r = recipe book
    deps <- subselect book (PureDepHolder book) (Proxy @deps)
    runRecipe r $ DepsComputed deps
  setTyped _ x = x

finishPure :: forall b target (book :: [Type]).
  ( HasRecipe Identity target book
  , SubSelect Identity book (RecipeDeps Identity target book) (PureDepHolder book)
  , HasTypes (RecipeDeps Identity target book) (DepsComputed (PureDepHolder book))
  , EverythingIsApplied Identity target book (RecipeDepsCalc Identity target book)
  , SOP.Generic b
  , Code b ~ '[book]
  ) => b -> target
finishPure book = getTyped @(target) (DepsComputed (PureDepHolder (extractBook book)))

extractBook :: forall a (l :: [Type]).
  ( SOP.Generic a
  , Code a ~ '[l]
  ) => a -> (NP I l)
  -- unsafeCoerce worked in the SOP Record example, seems to work here.
  -- TODO write some tests
extractBook = unZ . unSOP . from

emptyStore :: forall (deps :: [*]). (SListI deps) => NP Maybe deps
emptyStore = hpure Nothing

finish :: forall (effect :: Type -> Type) target (book :: [Type]) (store :: [Type]) b.
  ( store ~ (RecipeDepsCalc effect target book)
  , (SListI (RecipeDepsRec effect target book (RecipeDeps effect target book)))
  , HasRecipe effect target book
  , Monad effect
  , (SubSelect effect book (RecipeDeps effect target book) (NP Maybe store))
  , EverythingIsApplied effect target book (RecipeDepsCalc effect target book)
  , HasTypes (RecipeDeps effect target book) (DepsComputed (NP Maybe store))
  , SOP.Generic b
  , Code b ~ '[book]
  ) =>
  b -> effect target
finish book = do
  let
    store = emptyStore @store
  s <- bake (extractBook book) store (Proxy @target)
  pure $ getTyped @target (DepsComputed s)

instance (
  HasType (Maybe a) (NP Maybe deps)
  ) => HasType a (DepsComputed (NP Maybe deps)) where
  getTyped (DepsComputed m) = fromMaybe (error "No element of this type available. This should not happen, it should have been produced by an earlier bake. Please file a bug.") $ getTyped m
  setTyped element (DepsComputed m)= DepsComputed $ setTyped (Just element) m

instance {-# OVERLAPPABLE #-} (
  HasType (Maybe a) (NP Maybe tail)
  ) => HasType (Maybe a) (NP Maybe (h ': tail)) where
  getTyped np = getTyped $ tl np
  setTyped a np = hd np :* (setTyped a $ tl np)

instance {-# OVERLAPPING #-} HasType (Maybe a) (NP Maybe (a ': tail)) where
  getTyped np = hd np
  setTyped a np = a :* (tl np)
