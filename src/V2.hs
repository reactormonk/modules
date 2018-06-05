{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module V2 where

import Universum hiding (Nat)
import Data.Diverse
import Data.Diverse.Many
import Data.Diverse.TypeLevel
import qualified Data.Sequence as S
import Data.Diverse.Many.Internal (Many(..))
import Unsafe.Coerce
import Numbers
-- import GHC.TypeLits as Lits

pureRecipe :: Applicative effect => target -> Recipe effect target $ empty
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
      (AddLists
        (RecipeDeps effect hDeps book)
        (RecipeDepsRec effect target book tDeps)))
  RecipeDepsRec effect target book '[] = '[target]

newtype Recipe (effect :: * -> *) target (deps :: [*]) = Recipe { runRecipe :: Many deps -> effect target }

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

type EmptyStore effect target book = (LiftMaybe (Nub (RecipeDepsRec effect target book (RecipeDeps effect target book))))

emptyStore :: forall effect target book.
  (ToS (ListLen (EmptyStore effect target book))) =>
    Proxy effect -> Proxy target -> Proxy book -> Many (EmptyStore effect target book)
emptyStore _ _ _ =
  unsafeCoerce $ S.fromFunction len (const Nothing)
  where len :: Int = toLen (Proxy @(LiftMaybe (Nub (RecipeDepsRec effect target book (RecipeDeps effect target book)))))

findOrUpdate
    :: forall x xs f.
       (UniqueMember (Maybe x) xs , Applicative f)
    => Many xs -> f x -> f (Many xs, x)
findOrUpdate ls f =
  case grab ls :: Maybe x of
    Just x -> pure (ls, x)
    Nothing -> do
      x <- f
      pure (replace' ls (Just x), x)

class SubSelect effect (book :: [*]) (deps :: [*]) (state :: [*]) where
  subselect :: Many book -> Many state -> Proxy deps -> effect (Many state, Many deps)

instance forall effect book state dep depTail.
  (Monad effect, CanCook book state effect dep, SubSelect effect book depTail state) =>
  SubSelect effect book (dep ': depTail) state where
    subselect :: Many book -> Many state -> Proxy deps -> effect (Many state, Many (dep ': depTail))
    subselect book s1 _ = do
      (s2, target) <- cook book s1 (Proxy :: Proxy dep)
      (s3, (subDeps :: Many depTail)) <- subselect book s2 (Proxy :: Proxy depTail)
      pure $ (s3, (target :: dep) ./ subDeps)

instance (Monad effect) => SubSelect effect book '[] state where
  subselect _ state _ = pure (state, nil)

class CanCook (book :: [*]) (state :: [*]) effect target where
  cook :: Many book -> Many state -> Proxy target -> effect (Many state, target)

instance forall effect target book state.
  (HasRecipe effect target book,
   SubSelect effect book (RecipeDeps effect target book) state,
   Monad effect,
   UniqueMember (Maybe target) state) =>
  CanCook book state effect target where
    cook :: Many book -> Many state -> Proxy target -> effect (Many state, target)
    cook book s1 (Proxy :: Proxy target)= do
      let
        s2 :: effect (Many state, Many (RecipeDeps effect target book))
        s2 = subselect book s1 Proxy
        r :: Recipe effect target (RecipeDeps effect target book)
        r = recipe book
        build :: Many (RecipeDeps effect target book) -> effect target
        build d = runRecipe r $ d
        res :: Many state -> Many (RecipeDeps effect target book) -> effect (Many state, target)
        res s2 deps = findOrUpdate (s2 :: Many state) (build deps)
      (s2r, deps) :: (Many state, Many (RecipeDeps effect target book)) <- s2
      (res s2r deps) :: effect (Many state, target)

finish :: forall target (effect:: * -> *) (book :: [*]) (store :: [*]).
  ( store ~ (LiftMaybe (Nub (RecipeDepsRec effect target book (RecipeDeps effect target book))))
  , ToS (ListLen (EmptyStore effect target book))
  , HasRecipe effect target book
  , Monad effect
  , (SubSelect effect book (RecipeDeps effect target book) store)
  , (UniqueMember (Maybe target) store)
  ) =>
  Many book -> effect target
finish book = do
  let
    store = emptyStore (Proxy @effect) (Proxy @target) (Proxy @book)
    r :: Recipe effect target (RecipeDeps effect target book) = recipe book
  (_, target) <- cook book store (Proxy @target)
  pure target

-- test

class DefaultRecipe (effect :: * -> *) target where
  type DefaultRecipeDeps effect target :: [*]
  def :: Recipe effect target (DefaultRecipeDeps effect target)

instance DefaultRecipe effect target => HasRecipe effect target '[] where
  recipe _ = def
