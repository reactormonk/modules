{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module TestData where

import V2
import Universum hiding (Nat)
import qualified Data.Generics.Product as P

forceResult :: Proxy a -> Proxy a -> Proxy a
forceResult _ _ = Proxy

data T0 = T0 T1 T3
data T1 = T1 T2 T3
newtype T2 = T2 ()
newtype T3 = T3 T4
newtype T4 = T4 ()
newtype T5 = T5 T0

instance DefaultRecipe Identity T0 where
  type DefaultRecipeDeps Identity T0 = '[T1, T3]
  def = Recipe $ \deps -> pure $ T0 (getTyped deps) (getTyped deps)

instance DefaultRecipe Identity T1 where
  type DefaultRecipeDeps Identity T1 = '[T2, T3]
  def = Recipe $ \deps -> pure $ T1 (getTyped deps) (getTyped deps)

instance DefaultRecipe Identity T2 where
  type DefaultRecipeDeps Identity T2 = '[]
  def = Recipe $ \_ -> pure $ T2 ()

instance DefaultRecipe Identity T3 where
  type DefaultRecipeDeps Identity T3 = '[T4]
  def = Recipe $ \deps -> pure $ T3 (getTyped deps)

instance DefaultRecipe Identity T4 where
  type DefaultRecipeDeps Identity T4 = '[]
  def = Recipe $ \_ -> pure $ T4 ()

instance DefaultRecipe Identity T5 where
  type DefaultRecipeDeps Identity T5 = '[T0]
  def = Recipe $ \deps -> pure $ T5 (getTyped deps)
