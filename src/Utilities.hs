{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Utilities where

import Universum
import V2
import Generics.SOP as SOP
import Generics.SOP.Constraint
import Language.Haskell.TH

pureRecipe :: Applicative effect => target -> Recipe effect target '[]
pureRecipe target = Recipe $ \_ -> pure target

fromHasTypesToNP :: forall (l :: [*]) a.
  ( HasTypes a l
  ) => a -> NP I l
fromHasTypesToNP a = hcliftA (Proxy :: Proxy (HasType a)) (\_ -> I $ getTyped a) $ hpure Nothing

type GenericCachedRecipeDeps a = Head (Code a)

genericCachedRecipe :: forall a (l :: [*]) (effect :: * -> *).
  ( IsProductType a l
  , Applicative effect
  ) => Recipe effect a l
genericCachedRecipe = Recipe $ \deps ->
  pure $ to $ SOP $ Z $ ((fromHasTypesToNP deps) :: NP I l)

genericTransientRecipe :: forall a (l :: [*]) (m :: * -> *) (n :: * -> *).
  ( SOP.Generic a
  , IsProductType a l
  , Applicative m
  , Applicative n
  , Unlift (Lift m l) ~ l
  ) => Recipe n (m a) (Lift m l)
genericTransientRecipe = Recipe $ \deps ->
  pure $ map toA $ hsequence (moveLift ((fromHasTypesToNP deps) :: NP I (Lift m l)))
  where
    toA :: NP I l -> a
    toA = to . SOP . Z

genericTransientRecipeInstance :: Name -> DecsQ
genericTransientRecipeInstance typ =
  [d|
  instance (Applicative m, Applicative n) => DefaultRecipe m (n $(conT typ)) where
    type DefaultRecipeDeps m (n $(conT typ)) = Lift n (Head (Code $(conT typ)))
    def = genericTransientRecipe
  |]

genericCachedRecipeInstance :: Name -> Name -> DecsQ
genericCachedRecipeInstance effect typ =
  [d|
  instance DefaultRecipe $(conT effect) $(conT typ) where
    type DefaultRecipeDeps $(conT effect) $(conT typ) = Head (Code $(conT typ))
    def = genericCachedRecipe
  |]

-- Provided by lyxia

type family Lift (m :: k -> k) (l :: [k]) = r | r -> l where
  Lift m '[] = '[]
  Lift m (h : t) = ((m h) : (Lift m t))

type family Unlift (l :: [k]) where
  Unlift '[] = '[]
  Unlift (mh : t) = UnApply mh : Unlift t

type family UnApply (u :: k) where
  UnApply (m h) = h

moveLift :: (l' ~ Lift m l, l ~ Unlift l') => NP I l' -> NP m l
moveLift Nil = Nil
moveLift (I u :* v) = u :* (moveLift v)
