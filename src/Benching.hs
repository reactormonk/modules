{-# OPTIONS_GHC -freduction-depth=10000 #-}
module Benching where

import V2
import Universum
import BenchData
import Generics.SOP

c :: M10
c = finishPure ()

c40 :: M40
c40 = finishPure () -- (Proxy :: Proxy Store)

c49 :: Identity M49
c49 = finishStore (Proxy @Store) ()

type Store = NP Maybe '[M0, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32, M33, M34, M35, M36, M37, M38, M39, M40, M41, M42, M43, M44, M45, M46, M47, M48, M49]

instance Semigroup Store where
  (<>) = undefined

instance Monoid Store where
  mempty = hpure Nothing
