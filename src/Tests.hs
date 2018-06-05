{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Tests where

import V2
import Universum hiding (Nat)
import Data.Diverse
import Data.Diverse.Many
import Data.Diverse.TypeLevel
import qualified Data.Sequence as S
import Data.Diverse.Many.Internal (Many(..))
import Unsafe.Coerce
import Numbers

forceResult :: Proxy a -> Proxy a -> Proxy a
forceResult _ _ = Proxy

data M0 = M0 M1 M3
data M1 = M1 M2 M3
newtype M2 = M2 ()
newtype M3 = M3 M4
newtype M4 = M4 ()

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

l1 = forceResult (Proxy :: Proxy (AddLists '[M1, M2] '[M3, M4])) (Proxy :: Proxy '[M4, M3, M1, M2])

f34 = forceResult (Proxy :: Proxy (RecipeDepsRec Identity M3 '[] '[M4])) (Proxy :: Proxy '[M3, M4])

f4 = forceResult (Proxy :: Proxy (EmptyStore Identity M4 '[])) (Proxy :: Proxy '[Maybe M4])
f3 = forceResult (Proxy :: Proxy (EmptyStore Identity M3 '[])) (Proxy :: Proxy '[Maybe M3, Maybe M4])
f2 = forceResult (Proxy :: Proxy (EmptyStore Identity M2 '[])) (Proxy :: Proxy '[Maybe M2])
f1 = forceResult (Proxy :: Proxy (EmptyStore Identity M1 '[])) (Proxy :: Proxy '[Maybe M3, Maybe M1, Maybe M4, Maybe M2])

c4 :: Identity M4
c4 = finish nil

c3 :: Identity M3
c3 = finish nil

c2 :: Identity M2
c2 = finish nil

c1 :: Identity M1
c1 = finish nil

r1 :: Recipe Identity M3 '[]
r1 = Recipe $ \deps -> pure $ M3 (M4 ())

rc1 :: Identity M1
rc1 = finish (r1 ./ nil)
