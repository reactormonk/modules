{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
module V2 where

import Universum
import Data.Diverse
import Data.Diverse.Many
import Data.Diverse.TypeLevel
import qualified Data.Sequence as S
import Data.Diverse.Many.Internal (Many(..))
import Unsafe.Coerce
import GHC.TypeLits as Lits

pureRecipe :: Applicative effect => target -> Recipe effect target $ empty
pureRecipe target = Recipe $ \_ -> pure target

-- TODO
--  - first calculate the required storage for state via type family
--  - fill it with Nothing
--  - use as required
type family StateRequired (book :: [*]) (target :: *) effect (deps :: [*])

class CollectTypes (book :: [*]) (target :: *) effect (deps :: [*]) (depCache :: [*]) | book target effect deps -> depCache where
  emptyState :: Proxy book -> Proxy target -> Proxy effect -> Proxy deps -> Proxy types

instance (HasRecipe effect target book '[]) => CollectTypes book target effect '[] '[target] where
  emptyState _ _ _ _ = Proxy

instance (HasRecipe effect target book (depH ': depT)) => CollectTypes book target effect (depH ': depT) '[target] where
  emptyState _ _ _ _ = Proxy

class CollectSubTypes (book :: [*]) (targets :: [*]) effect (depCache :: [*]) | book targets effect -> depCache where
  collectSubTypes :: Proxy book -> Proxy targets -> Proxy effect -> Proxy depCache

instance CollectSubTypes book '[] effect '[] where
  collectSubTypes _ _ _ = Proxy

instance (CollectTypes book effect hTarget deps hCache, CollectSubTypes book tTargets effect tCache, (AddLists hCache tCache) ~ depCache) => CollectSubTypes book (hTarget ': tTargets) effect depCache where
  collectSubTypes :: Proxy book -> Proxy (hTarget ': tTargets) -> Proxy effect -> Proxy (AddLists hCache tCache)
  collectSubTypes _ _ _ = Proxy :: Proxy (AddLists hCache tCache)

type family AddLists (l1 :: [k]) (l2 :: [k]) :: [k] where
  AddLists '[] l2 = l2
  AddLists l1 '[] = l1
  AddLists (h1 ': t1) l2 = AddLists t1 (h1 ': l2)

-- copypastad from postfix', so it should work as expected
postfixLifted
    :: forall y xs f.
       (MaybeUniqueMember y xs , Applicative f)
    => Many xs -> f y -> f (Many (SnocUnique xs y), y)
postfixLifted (Many ls) fy = if i /= 0 then pure (Many ls, unsafeCoerce (S.index ls i)) else (\y -> (Many (ls S.|> unsafeCoerce y), y)) <$> fy
  where
    i = fromInteger (Lits.natVal @(PositionOf y xs) Proxy) :: Int

newtype Recipe (effect :: * -> *) target (deps :: [*]) = Recipe { runRecipe :: Many deps -> effect target }

class HasRecipe (effect :: * -> *) target (book :: [*]) (deps :: [*]) | target effect book -> deps where
  recipe :: Many book -> Recipe effect target deps

instance HasRecipe effect target ((Recipe effect target deps) ': tail) deps where
  recipe list = front list
instance HasRecipe effect target tail deps => HasRecipe effect target (head ': tail) deps where
  recipe list = recipe $ aft list

class SubSelect effect (book :: [*]) (deps :: [*]) (state :: [*]) (state1 :: [*]) | effect book deps state -> state1 where
  subselect :: Many book -> Many state -> Proxy deps -> effect (Many state1, Many deps)

instance forall effect book state1 state2 state3 dep depTail.
  (Monad effect, CanCook book state1 state2 effect dep, SubSelect effect book depTail (SnocUnique state2 dep) state3) =>
  SubSelect effect book (dep ': depTail) state1 state3 where
    subselect :: Many book -> Many state1 -> Proxy deps -> effect (Many state3, Many (dep ': depTail))
    subselect book s1 _ = do
      (s2, target) <- cook book s1 (Proxy :: Proxy dep)
      (s3, (subDeps :: Many depTail)) <- subselect book s2 (Proxy :: Proxy depTail)
      pure $ (s3, (target :: dep) ./ subDeps)

instance (Monad effect) => SubSelect effect book '[] state1 state1 where
  subselect _ state _ = pure (state, nil)

class CanCook (book :: [*]) (state1 :: [*]) (state2 :: [*]) effect target | book state1 effect target -> state2 where
  cook :: Many book -> Many state1 -> Proxy target -> effect (Many (SnocUnique state2 target), target)

instance forall effect target book deps state1 state2.
  (HasRecipe effect target book deps, SubSelect effect book deps state1 state2, Monad effect, MaybeUniqueMember target state2) =>
  CanCook book state1 state2 effect target where
    cook :: Many book -> Many state1 -> Proxy target -> effect (Many (SnocUnique state2 target), target)
    cook book s1 (Proxy :: Proxy target)= do
      let
        s2 = (subselect book s1 (Proxy :: Proxy deps)) :: effect (Many state2, Many deps)
        r :: Recipe effect target deps
        r = recipe book
        build :: Many deps -> effect target
        build d = runRecipe r $ d
        res :: Many state2 -> Many deps -> effect (Many (SnocUnique state2 target), target)
        res s2 deps = postfixLifted (s2 :: Many state2) (build deps)
      (s2r, deps) :: (Many state2, Many deps) <- s2
      (res s2r deps) :: effect (Many (SnocUnique state2 target), target)

-- finish :: (Monad effect) => Many book -> Proxy target -> effect target
-- finish book p = do
--   (_, t) <- cook book nil p
--   pure t

-- test

type family DefaultRecipeFamily (book :: [*]) (effect :: * -> *) (target :: *) :: deps

class DefaultRecipe (effect :: * -> *) target (deps :: [*]) | effect target -> deps where
  def :: Recipe effect target deps

instance DefaultRecipe effect target deps => HasRecipe effect target '[] deps where
  recipe _ = def

data M0 = M0 M1 M3
data M1 = M1 M2 M3
newtype M2 = M2 ()
newtype M3 = M3 M4
newtype M4 = M4 ()

instance DefaultRecipe Identity M0 '[M1, M3] where
  def = Recipe $ \deps -> pure $ M0 (grab deps) (grab deps)

type instance DefaultRecipeFamily '[] Identity M0 = '[M1, M3]
type instance DefaultRecipeFamily ((Recipe Identity M0 deps)': t) Identity M0 = deps
type instance DefaultRecipeFamily ((Recipe e a d) ': t) Identity M0 = DefaultRecipeFamily t Identity M0

instance DefaultRecipe Identity M1 '[M2, M3] where
  def = Recipe $ \deps -> pure $ M1 (grab deps) (grab deps)

type instance DefaultRecipeFamily '[] Identity M1 = '[M1, M3]

instance DefaultRecipe Identity M2 '[] where
  def = Recipe $ \deps -> pure $ M2 ()

type instance DefaultRecipeFamily '[] Identity M2 = '[]

instance DefaultRecipe Identity M3 '[M4] where
  def = Recipe $ \deps -> pure $ M3 (grab deps)

type instance DefaultRecipeFamily '[] Identity M3 = '[M4]

instance DefaultRecipe Identity M4 '[] where
  def = Recipe $ \deps -> pure $ M4 ()

type instance DefaultRecipeFamily '[] Identity M4 = '[]
