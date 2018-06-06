{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Tests where

import V2
import TestData
import Universum hiding (Nat)
import Data.Diverse
import Data.Diverse.Many
import Data.Diverse.TypeLevel
import qualified Data.Sequence as S
import Data.Diverse.Many.Internal (Many(..))
import Unsafe.Coerce
import Numbers

-- l1 = forceResult (Proxy :: Proxy (AddLists '[M1, M2] '[M3, M4])) (Proxy :: Proxy '[M4, M3, M1, M2])

-- f34 = forceResult (Proxy :: Proxy (RecipeDepsRec Identity M3 '[] '[M4])) (Proxy :: Proxy '[M4, M3, M3])

-- f4 = forceResult (Proxy :: Proxy (EmptyStore Identity M4 '[])) (Proxy :: Proxy '[Maybe M4])
-- f3 = forceResult (Proxy :: Proxy (EmptyStore Identity M3 '[])) (Proxy :: Proxy '[Maybe M4, Maybe M3])
-- f2 = forceResult (Proxy :: Proxy (EmptyStore Identity M2 '[])) (Proxy :: Proxy '[Maybe M2])
-- f1 = forceResult (Proxy :: Proxy (EmptyStore Identity M1 '[])) (Proxy :: Proxy '[Maybe M2, Maybe M3, Maybe M4, Maybe M1])

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

r5 :: Recipe Identity M5 '[M0]
r5 = Recipe $ \deps -> pure $ M5 (grab deps)

c5 :: Identity M5
c5 = finish nil

rc5 :: Identity M5
rc5 = finish (r5 ./ nil)

-- Should fail with "not everything is applied"
-- rc4fail :: Identity M4
-- rc4fail = finish (r5 ./ nil)
