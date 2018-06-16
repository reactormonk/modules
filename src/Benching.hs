{-# OPTIONS_GHC -freduction-depth=1000 #-}
module Benching where

import V2
import Universum
import Data.Diverse.Many
import BenchData

-- c :: M10
-- c = runIdentity $ finishDD nil

c49 :: M49
c49 = runIdentity $ finishDD nil -- (Proxy :: Proxy Store)
