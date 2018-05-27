{-# LANGUAGE UndecidableInstances #-}
module Lib where

import Universum
import Data.HList

pureRecipe :: Applicative effect => target -> Recipe effect target '[]
pureRecipe target = Recipe $ \HNil -> pure target

newtype Recipe effect target (deps :: [*]) = Recipe { runRecipe :: HList deps -> effect target }

class DefaultRecipe target where
  def :: Recipe target deps effect

class CanCook target (pot :: [*]) (deps :: [*]) | target pot -> deps where
  cook :: HList pot -> (PotEffect pot) target

instance (HasRecipe target pot deps, SubSelect pot deps, Monad (PotEffect pot)) => CanCook target pot deps where
  cook pot = do
    deps <- (subselect pot :: (PotEffect pot) (HList deps))
    let
      r :: Recipe (PotEffect pot) target deps
      r = recipe pot
    runRecipe r $ (deps :: HList deps)

type family PotEffect (pot :: [*]) :: * -> *
type instance PotEffect (Recipe effect _ _ ': '[]) = effect
type instance PotEffect (Recipe effect _ _ ': Recipe effect _ _ ': '[]) = effect
type instance PotEffect (Recipe effect _ _ ': Recipe effect _ _ ': Recipe effect _ _ ': '[]) = effect
type instance PotEffect (Recipe effect _ _ ': Recipe effect _ _ ': Recipe effect _ _ ': Recipe effect _ _ ': '[]) = effect
type instance PotEffect (Recipe effect _ _ ': Recipe effect _ _ ': Recipe effect _ _ ': Recipe effect _ _ ': Recipe effect _ _ ': '[]) = effect

class HasRecipe target (pot :: [*]) (deps :: [*]) | target pot -> deps where
  recipe :: HList pot -> Recipe (PotEffect pot) target deps

class Monad (PotEffect pot) => SubSelect (pot :: [*]) (deps :: [*]) where
  subselect :: HList pot -> (PotEffect pot) (HList deps)

instance (Monad (PotEffect pot), CanCook dep pot deps, SubSelect pot depTail) => SubSelect pot (dep ': depTail) where
  subselect pot = do
    d <- cook pot
    t <- subselect pot
    pure $ HCons d t

instance Monad (PotEffect pot) => SubSelect pot '[] where
  subselect _ = pure HNil

instance {-# OVERLAPPING #-} (PotEffect ((Recipe effect target deps) ': leftoverPot) ~ effect) =>
    HasRecipe target ((Recipe effect target deps) ': leftoverPot) deps where
  recipe (HCons h _) = h

instance (HasRecipe target leftoverPot deps, PotEffect ((Recipe effect target1 deps1) ': leftoverPot) ~ effect, PotEffect leftoverPot ~ effect) =>
    HasRecipe target ((Recipe effect target1 deps1) ': leftoverPot) deps where
  recipe (HCons _ t) = recipe t

data M0 = M0 M1 M3
data M1 = M1 M2 M3
newtype M2 = M2 ()
newtype M3 = M3 M4
newtype M4 = M4 ()

r0 :: Recipe IO M0 '[M1, M3]
r0 = Recipe $ \(HCons m1 (HCons m3 HNil)) -> print "m1 constructed" >> (pure $ M0 m1 m3)

r1 :: Recipe IO M1 '[M2, M3]
r1 = Recipe $ \(HCons m2 (HCons m3 HNil)) -> print "m1 constructed" >> (pure $ M1 m2 m3)

r2 :: Recipe IO M2 '[]
r2 = Recipe $ \_ -> print "m2 constructed" >> (pure $ M2 ())

r3 :: Recipe IO M3 '[M4]
r3 = Recipe $ \(HCons m4 HNil) -> print "m3 constructed" >> (pure $ M3 m4)

r4 :: Recipe IO M4 '[]
r4 = Recipe $ \_ -> print "m4 constructed" >> (pure $ M4 ())

cookbook1 = r0 .*. r1 .*. r2 .*. r3 .*. r4 .*. HNil

cookbook2 = r3 .*. r4 .*. r2 .*. r1 .*. HNil

c1 = cook cookbook1 :: IO M0
