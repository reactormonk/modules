{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Main where

import V2
import TestData
import Universum hiding (Nat)

main :: IO ()
main = pure ()

-- l1 = forceResult (Proxy :: Proxy (AddLists '[M1, M2] '[M3, M4])) (Proxy :: Proxy '[M4, M3, M1, M2])

-- f34 = forceResult (Proxy :: Proxy (RecipeDepsRec Identity M3 '[] '[M4])) (Proxy :: Proxy '[M4, M3, M3])

-- f4 = forceResult (Proxy :: Proxy (EmptyStore Identity M4 '[])) (Proxy :: Proxy '[Maybe M4])
-- f3 = forceResult (Proxy :: Proxy (EmptyStore Identity M3 '[])) (Proxy :: Proxy '[Maybe M4, Maybe M3])
-- f2 = forceResult (Proxy :: Proxy (EmptyStore Identity M2 '[])) (Proxy :: Proxy '[Maybe M2])
-- f1 = forceResult (Proxy :: Proxy (EmptyStore Identity M1 '[])) (Proxy :: Proxy '[Maybe M2, Maybe M3, Maybe M4, Maybe M1])

pure4 :: T4
pure4 = finishPure ()

pure3 :: T3
pure3 = finishPure ()

pure2 :: T2
pure2 = finishPure ()

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
