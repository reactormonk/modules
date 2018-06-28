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

c4 :: Identity T4
c4 = finishDD nil

c3 :: Identity T3
c3 = finishDD nil

c2 :: Identity T2
c2 = finishDD nil

c1 :: Identity T1
c1 = finishDD nil

r1 :: Recipe Identity T3 '[]
r1 = Recipe $ \deps -> pure $ T3 (T4 ())

rc1 :: Identity T1
rc1 = finishDD (r1 ./ nil)

r5 :: Recipe Identity T5 '[T0]
r5 = Recipe $ \deps -> pure $ T5 (getTyped deps)

c5 :: Identity T5
c5 = finishDD nil

rc5 :: Identity T5
rc5 = finishDD (r5 ./ nil)

pure4 :: T4
pure4 = finishPure nil

pure3 :: T3
pure3 = finishPure nil

pure2 :: T2
pure2 = finishPure nil

-- pure1 :: T1
-- pure1 = finishPure nil

-- pure0 :: T0
-- pure0 = finishPure nil

-- Should fail with "not everything is applied"
-- rc4fail :: Identity M4
-- rc4fail = finish (r5 ./ nil)

-- pre4 :: Identity T4
-- pre4 = finish nil (Proxy :: Proxy Store)

-- pre3 :: Identity T3
-- pre3 = finish nil (Proxy :: Proxy Store)

-- pre1 :: Identity T1
-- pre1 = finish nil (Proxy :: Proxy Store)

-- pre0 :: Identity T0
-- pre0 = finish nil (Proxy :: Proxy Store)

-- prepareFinishTH @Identity @T4 @'[]
