{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module TestData where

import V2
import Universum hiding (Nat)
import Data.Diverse

forceResult :: Proxy a -> Proxy a -> Proxy a
forceResult _ _ = Proxy

data M0 = M0 M1 M3
data M1 = M1 M2 M3
newtype M2 = M2 ()
newtype M3 = M3 M4
newtype M4 = M4 ()
newtype M5 = M5 M0

instance DefaultRecipe Identity M0 where
  type DefaultRecipeDeps Identity M0 = '[M1, M3]
  def = Recipe $ \deps -> pure $ M0 (grab deps) (grab deps)

instance DefaultRecipe Identity M1 where
  type DefaultRecipeDeps Identity M1 = '[M2, M3]
  def = Recipe $ \deps -> pure $ M1 (grab deps) (grab deps)

instance DefaultRecipe Identity M2 where
  type DefaultRecipeDeps Identity M2 = '[]
  def = Recipe $ \deps -> pure $ M2 ()

instance DefaultRecipe Identity M3 where
  type DefaultRecipeDeps Identity M3 = '[M4]
  def = Recipe $ \deps -> pure $ M3 (grab deps)

instance DefaultRecipe Identity M4 where
  type DefaultRecipeDeps Identity M4 = '[]
  def = Recipe $ \deps -> pure $ M4 ()

instance DefaultRecipe Identity M5 where
  type DefaultRecipeDeps Identity M5 = '[M0]
  def = Recipe $ \deps -> pure $ M5 (grab deps)
