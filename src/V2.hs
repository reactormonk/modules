{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
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

class SubSelect effect (book :: [*]) (deps :: [*]) (state :: [*]) (state1 :: [*]) | effect book deps state -> state1, effect book -> deps where
  subselect :: Many book -> Many state -> Proxy deps -> effect (Many state1, Many deps)

instance forall effect book state1 state2 state3 dep depTail.
  (Monad effect, CanCook book state1 state2 effect dep, SubSelect effect book depTail (SnocUnique state2 dep) state3) =>
  SubSelect effect book (dep ': depTail) state1 state3 where
    subselect :: Many book -> Many state1 -> Proxy deps -> effect (Many state3, Many (dep ': depTail))
    subselect book s1 _ = do
      (s2, target) <- cook book s1 (Proxy :: Proxy dep)
      (s3, (subDeps :: Many depTail)) <- subselect book s2 (Proxy :: Proxy depTail)
      pure $ (s3, (target :: dep) ./ subDeps)

instance (Monad effect, state1 ~ state) => SubSelect effect book '[] state state1 where
  subselect _ state _ = pure (state, nil)

class CanCook (book :: [*]) (state :: [*]) (state1 :: [*]) effect target | book state effect target -> state1 where
  cook :: Many book -> Many state -> Proxy target -> effect (Many (SnocUnique state1 target), target)

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

finish :: (Monad effect) => Many book -> Proxy target -> effect target
finish book p = do
  (_, t) <- cook book nil p
  pure t

-- test

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

instance DefaultRecipe Identity M1 '[M2, M3] where
  def = Recipe $ \deps -> pure $ M1 (grab deps) (grab deps)

instance DefaultRecipe Identity M2 '[] where
  def = Recipe $ \deps -> pure $ M2 ()

instance DefaultRecipe Identity M3 '[M4] where
  def = Recipe $ \deps -> pure $ M3 (grab deps)

instance DefaultRecipe Identity M4 '[] where
  def = Recipe $ \deps -> pure $ M4 ()
