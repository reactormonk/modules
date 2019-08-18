module Main where

import DD
import V2
import Generics.SOP (I(..))
import Universum
import TestData

main :: IO ()
main = pure ()

c4 :: Identity T4
c4 = finishDD ()

c3 :: Identity T3
c3 = finishDD ()

c2 :: Identity T2
c2 = finishDD ()

c1 :: Identity T1
c1 = finishDD ()

rc1 :: Identity T1
rc1 = finishDD (I r1)

c5 :: Identity T5
c5 = finishDD ()

rc5 :: Identity T5
rc5 = finishDD (I r5)

r1 :: Recipe Identity T3 '[]
r1 = Recipe $ \_ -> pure $ T3 (T4 ())

r5 :: Recipe Identity T5 '[T0]
r5 = Recipe $ \deps -> pure $ T5 (getTyped deps)
