{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module TestData where

import V2
import Universum hiding (Nat)
import Data.Diverse
import qualified Data.Generics.Product as P

forceResult :: Proxy a -> Proxy a -> Proxy a
forceResult _ _ = Proxy

data T0 = T0 T1 T3
data T1 = T1 T2 T3
newtype T2 = T2 ()
newtype T3 = T3 T4
newtype T4 = T4 ()
newtype T5 = T5 T0

data Store = Store
  { m0 :: Maybe T0
  , m1 :: Maybe T1
  , m2 :: Maybe T2
  , m3 :: Maybe T3
  , m4 :: Maybe T4
  , m5 :: Maybe T5
  } deriving (Generic)

instance P.HasType a Store => HasType a Store where
  getTyped = P.getTyped
  setTyped = P.setTyped

instance Semigroup Store where
  Store l0 l1 l2 l3 l4 l5 <> Store r0 r1 r2 r3 r4 r5 = Store (l0 <|> r0) (l1 <|> r1) (l2 <|> r2) (l3 <|> r3) (l4 <|> r4) (l5 <|> r5)

instance Monoid Store where
  mempty = Store Nothing Nothing Nothing Nothing Nothing Nothing
  mappend = (<>)

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
