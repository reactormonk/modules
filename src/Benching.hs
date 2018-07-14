{-# OPTIONS_GHC -freduction-depth=1000 #-}
module Benching where

import V2
import Universum
import BenchData

c :: M10
c = finishPure ()

c49 :: M49
c49 = finishPure () -- (Proxy :: Proxy Store)
